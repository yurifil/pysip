from pysip import PySIPException
from pysip.message.hdr import Header, BaseSipHeader
from pysip.message.nameaddr import NameAddress, NameAddressError
from pysip.message.parser_aux import parse_params
from pysip.uri import PARAM_LR


class RouteHeaderError(PySIPException):
    pass


class Route(object):
    def __init__(self, nameaddr, params):
        self.nameaddr = nameaddr
        self.params = params
        self.uri = self.nameaddr.uri
        self.display_name = self.nameaddr.display_name

    def __eq__(self, other):
        if isinstance(other, Route):
            return self.nameaddr == other.nameaddr and self.params == other.params
        return NotImplemented

    def is_loose_route(self):
        return PARAM_LR in self.uri.params

    def assemble(self):
        nameaddr = NameAddress.assemble(self.display_name, self.uri)
        params_list = list()
        for k, v in self.params:
            if not v:
                params_list.append('{k}')
            else:
                params_list.append(f'{k}={v}')
        params = ';'.join(params_list)
        if params:
            params = f';{params}'
        return f'{nameaddr}{params}'


class RouteHeader(BaseSipHeader):
    def __init__(self, route_header=None):
        self.route_set = list()
        if route_header is not None:
            self.route_set = self.parse_route_header(route_header)

    def __eq__(self, other):
        if isinstance(other, RouteHeader):
            return self.route_set == other.route_set
        return NotImplemented

    @staticmethod
    def parse(header):
        return RouteHeader(header)

    @staticmethod
    def parse_route_header(route_header):
        if isinstance(route_header, Header):
            route_set = list()
            for val in route_header.values:
                nameaddr, parsed_params = RouteHeader.parse_route(val)
                route_set.append(Route(nameaddr, parsed_params))
            return route_set
        elif isinstance(route_header, str):
            nameaddr, parsed_params = RouteHeader.parse_route(route_header)
            return [Route(nameaddr, parsed_params)]
        else:
            raise RouteHeaderError(f'Cannot parse route {route_header}: should be type Header not {type(route_header)}')

    @staticmethod
    def parse_route(route):
        try:
            nameaddr = NameAddress(route)
            parsed_params = RouteHeader.parse_route_params(nameaddr.rest.strip())
            return nameaddr, parsed_params
        except NameAddressError as e:
            raise RouteHeaderError(f'Cannot parse route {route}: invalid nameaddress part ({e})')
        except PySIPException as e:
            raise RouteHeaderError(f'Cannot parse route {route}: {e}')

    @staticmethod
    def parse_route_params(params):
        if not params:
            return []
        if params.startswith(';'):
            return parse_params(params[1:], ';')
        else:
            raise RouteHeaderError(f'Cannot parse route parameters {params}: should start with ";"')

    @property
    def first(self):
        if self.route_set:
            return self.route_set[0]
        else:
            return None

    def build(self, header_name):
        hdr = Header(header_name)
        for r in self.route_set:
            hdr.add_value(r.assemble())
        return hdr
'''
-spec make_route(binary() | ersip_uri:uri()) -> route().
make_route(Bin) when is_binary(Bin) ->
    case parse_route(Bin) of
        {ok, Route} ->
            Route;
        {error, _} = Error ->
            error(Error)
    end;
make_route(URI) ->
    #route{display_name = {display_name, []}, uri = URI}.

-spec assemble_route(route()) -> iolist().
assemble_route(#route{} = Route) ->
    #route{display_name = DN,
           uri = URI,
           params = ParamsList
          } = Route,
    [ersip_nameaddr:assemble(DN, URI),
     lists:map(fun({Key, Value}) when is_binary(Value) ->
                       [<<";">>, Key, <<"=">>, Value];
                  ({Key, novalue})  ->
                       [<<";">>, Key]
               end,
               ParamsList)
    ].

parse_route_params(<<$;, Bin/binary>>) ->
    parse_route_params(Bin);
parse_route_params(<<>>) ->
    {ok, [], <<>>};
parse_route_params(Bin) ->
    case ersip_parser_aux:parse_params($;, Bin) of
        {ok, Params, <<>>} ->
            do_parse_params(Params, []);
        _ ->
            {error, {invalid_parameters, Bin}}
    end.

-spec do_parse_params(ersip_parser_aux:gen_param_list(), ersip_parser_aux:gen_param_list()) ->
                             ersip_parser_aux:parse_result(ersip_parser_aux:gen_param_list()).
do_parse_params([], Acc) ->
    {ok, lists:reverse(Acc), <<>>};
do_parse_params([{Key, <<>>} | Rest], Acc) ->
    Acc1 = [{Key, novalue} | Acc],
    do_parse_params(Rest, Acc1);
do_parse_params([{Key, Value} | Rest], Acc) ->
    Acc1 = [{Key, Value} | Acc],
    do_parse_params(Rest, Acc1).

add_to_maybe_route_set(Bin, {ok, RouteSet}) ->
    case parse_route(Bin) of
        {ok, Route} ->
            {ok, ersip_route_set:add_first(Route, RouteSet)};
        {error, _} = Error ->
            Error
    end.

-spec parse_route(binary()) -> parse_result().
parse_route(Bin) ->
    Parsers = [fun ersip_nameaddr:parse/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_route_params/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [{DisplayName, URI}, _, ParamsList], <<>>} ->
            {ok,
             #route{display_name = DisplayName,
                    uri          = URI,
                    params       = ParamsList
                   }
            };
        {error, _} = Error ->
            Error
    end.
    
parse(Header) ->
    MaybeRevRouteSet =
        lists:foldl(fun(IORoute, Acc) ->
                            add_to_maybe_route_set(iolist_to_binary(IORoute), Acc)
                    end,
                    {ok, ersip_route_set:new()},
                    ersip_hdr:raw_values(Header)),
    case MaybeRevRouteSet of
        {ok, RevRouteSet} ->
            {ok, ersip_route_set:reverse(RevRouteSet)};
        Error ->
            Error
    end.

%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Route/Record-route headers
%%

-module(ersip_hdr_route).

-export([uri/1,
         is_loose_route/1,
         params/1,
         set_param/3,
         make/1,
         parse/1,
         build/2,
         make_route/1
        ]).

-export_type([route/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(route, {display_name :: ersip_nameaddr:display_name(),
                uri          :: ersip_uri:uri(),
                params = []  :: [route_param()]
               }).
-type route()     :: #route{}.
-type route_set() :: ersip_route_set:route_set().
-type route_param() :: {Key :: binary(), Value :: binary()}.
-type parse_result() :: {ok, route_set()}
                      | {error, term()}.

-type maybe_rev_route_set() :: {ok, route_set()}
                             | {error, term()}.
%%%===================================================================
%%% API
%%%===================================================================

-spec uri(route()) -> ersip_uri:uri().
uri(#route{uri = URI}) ->
    URI.

-spec is_loose_route(route()) -> boolean().
is_loose_route(#route{uri = URI}) ->
    URIParams  = ersip_uri:params(URI),
    maps:is_key(lr, URIParams).

-spec params(route()) -> [route_param()].
params(#route{params = P}) ->
    P.

-spec set_param(Key :: binary(), Value :: binary(), route()) -> route().
set_param(Key, Value, #route{params = Params} = Route)
        when is_binary(Key), is_binary(Value) ->
    Route#route{params = [{Key, Value} | Params]}.

-spec make(iolist()) -> route_set().
make(Binary) ->
    H0 = ersip_hdr:new(<<"Route">>),
    H1 = ersip_hdr:add_value(Binary, H0),
    case parse(H1) of
        {ok, RouteSet} ->
            RouteSet;
        {error, _} = Error  ->
            error(Error)
    end.

-spec parse(ersip_hdr:header()) -> parse_result().



-spec build(HeaderName :: binary(), route_set()) -> ersip_hdr:header().
build(HdrName, {route_set, _} = RouteSet) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_route_set:foldl(
      fun(Route, HdrAcc) ->
              ersip_hdr:add_value(assemble_route(Route), HdrAcc)
      end,
      Hdr,
      RouteSet).

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec add_to_maybe_route_set(binary(), maybe_rev_route_set()) -> maybe_rev_route_set().
add_to_maybe_route_set(_, {error, _} = Error) ->
    Error;

'''