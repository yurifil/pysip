from pysip import PySIPException
from pysip.message.hdr import Header
from pysip.message.parser_aux import parse_token, parse_slash, parse_params


class MIMEType(object):
    MIME = 'mime'

    def __init__(self, type, subtype):
        self.type = type
        self.subtype = subtype

    def __eq__(self, other):
        if isinstance(other, MIMEType):
            return self.type == other.type and self.subtype == other.subtype


class ContentTypeHeaderError(PySIPException):
    pass


class ContentTypeHeader(object):
    def __init__(self, header=None):
        self.mime_type = None
        self.params = None
        if header is not None:
            self.mime_type, self.params = ContentTypeHeader.parse(header)

    @staticmethod
    def parse(header):
        if isinstance(header, str):
            return ContentTypeHeader.parse_content_type_string(header)
        elif isinstance(header, Header):
            return ContentTypeHeader.parse_header_object(header)
        else:
            raise ContentTypeHeaderError(f'Cannot parse header {header}: type of object is {type(header)}')

    @staticmethod
    def parse_content_type_string(header):
        try:
            content_type, rest = parse_token(header)
            slash, rest = parse_slash(rest)
            subtype, rest = parse_token(rest)
        except Exception as e:
            raise ContentTypeHeaderError(f'Cannot parse "{header}": invalid header ({e}).')
        if rest and rest.startswith(';'):
            params = parse_params(rest[1:], ';')
        else:
            if rest:
                raise ContentTypeHeaderError(f'Cannot parse "{header}": invalid parameters.')
            params = None
        return MIMEType(content_type, subtype), params

    @staticmethod
    def parse_header_object(header):
        raw_values = header.values.copy()
        if not raw_values:
            raise ContentTypeHeaderError(f'Cannot parse header "{header}": no content type.')
        else:
            return ContentTypeHeader.parse_content_type_string(''.join(header.values))

    def assemble(self):
        ret_val = f'{self.mime_type.type}/{self.mime_type.subtype}'
        if self.params is not None:
            for name, value in self.params:
                ret_val = f'{ret_val};{name}={value}'
        return ret_val

    def build(self, header_name='Content-Type'):
        hdr = Header(header_name)
        hdr.add_value(self.assemble())
        return hdr

'''
-spec build(HeaderName :: binary(), content_type()) -> ersip_hdr:header().
build(HdrName, #content_type{} = ContentType) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([assemble(ContentType)], Hdr).

-spec assemble(content_type()) -> iolist().
assemble(#content_type{} = ContentType) ->
    {mime, Type, SubType} = mime_type(ContentType),
    [Type, <<"/">>, SubType,
     lists:map(fun({Key, Value}) ->
                       [$;, Key, $=, Value]
               end,
               params(ContentType))
    ].

parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_content_type};
        [Content_TypeIOList]  ->
            parse_content_type(iolist_to_binary(Content_TypeIOList));
        _ ->
            {error, multiple_content_types}
    end.

parse_content_type(Binary) ->
    Parsers = [fun ersip_parser_aux:parse_token/1,
               fun ersip_parser_aux:parse_slash/1,
               fun ersip_parser_aux:parse_token/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_params/1
              ],
    case ersip_parser_aux:parse_all(Binary, Parsers) of
        {ok, [ Type, _, SubType, _, ParamsList], <<>>} ->
            {ok,
             #content_type{type    = {mime,
                                      ersip_bin:to_lower(Type),
                                      ersip_bin:to_lower(SubType)},
                           params  = ParamsList
                          }
            };
        _ ->
            {error, {invalid_content_type, Binary}}
    end.

parse_params(<<$;, Bin/binary>>) ->
    parse_params(Bin);
parse_params(<<>>) ->
    {ok, [], <<>>};
parse_params(Bin) ->
    ersip_parser_aux:parse_params($;, Bin).

-spec make(ersip_hdr:header() | binary()) -> content_type().
make(Bin) when is_binary(Bin) ->
    case parse_content_type(Bin) of
        {ok, Content_Type} ->
            Content_Type;
        Error ->
            error(Error)
    end;
make(Header) ->
    case parse(Header) of
        {ok, Content_Type} ->
            Content_Type;
        Error ->
            error(Error)
    end.


-spec parse(ersip_hdr:header()) -> Result when
      Result :: {ok, content_type()}
              | {error, Error},
      Error :: no_content_type
             | {invalid_content_type, binary()}.


%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Content-type header
%%

-module(ersip_hdr_content_type).

-export([mime_type/1,
         params/1,
         make/1,
         parse/1,
         build/2,
         assemble/1
        ]).

-export_type([content_type/0,
              mime_type/0 
             ]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(content_type, {
          type    :: mime_type(),
          params  :: params()
         }).
-type mime_type() :: {mime, Type :: binary(), SubType :: binary()}.
-type content_type() :: #content_type{}.
-type params() :: [pair()].
-type pair() :: {Key :: binary(), Value :: binary()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec mime_type(content_type()) -> mime_type().
mime_type(#content_type{type = T}) -> 
    T.

-spec params(content_type()) -> params().
params(#content_type{params = P}) -> 
    P.



%%%===================================================================
%%% Internal implementation
%%%===================================================================

%% media-type     =  m-type SLASH m-subtype *(SEMI m-parameter)
%% m-type           =  discrete-type / composite-type
%% discrete-type    =  "text" / "image" / "audio" / "video"
%%                     / "application" / extension-token
%% composite-type   =  "message" / "multipart" / extension-token
%% extension-token  =  ietf-token / x-token
%% ietf-token       =  token
%% x-token          =  "x-" token
%% m-subtype        =  extension-token / iana-token
%% iana-token       =  token
%% m-parameter      =  m-attribute EQUAL m-value
%% m-attribute      =  token
%% m-value          =  token / quoted-string
-spec parse_content_type(binary()) -> Result when
      Result :: {ok, content_type()}
              | {error, Error},
      Error  :: {invalid_content_type, binary()}.


'''