from pysip import PySIPException
from pysip.message.hdr import Header
from pysip.message.method import Method
from pysip.message.method_set import MethodSet


class AllowError(PySIPException):
    pass


class AllowHeader(object):
    def __init__(self, method_set):
        self.method_set = method_set

    def build_header(self, header_name):
        h = Header(header_name)
        h.add_value(', '.join(self.method_set.to_list()))


def parse_header(header):
    if not header.values:
        raise AllowError(f'Cannot parse header {header}: no values.')
    method_list = list()
    for val in header.values:
        try:
            method_list.append(Method(val))
        except (ValueError, PySIPException) as e:
            raise AllowError(f'{e}')
    return AllowHeader(method_set=MethodSet(method_list))


'''

-export([from_list/1,
         to_list/1,
         from_method_set/1,
         parse/1,
         build/2,
         assemble/1
        ]).
-export_type([allow/0]).

-type allow() :: {allow, ersip_method_set:set()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec from_list([ersip_method:method()]) -> allow().
from_list(MethodList) ->
    {allow, ersip_method_set:new(MethodList)}.

-spec to_list(allow()) -> [ersip_method:method()].
to_list({allow, MethodSet}) ->
    ersip_method_set:to_list(MethodSet).

-spec from_method_set(ersip_method_set:set()) -> allow().
from_method_set({method_set, _} = MethodSet) ->
    {allow, MethodSet}.

-spec parse(ersip_hdr:header()) -> Result when
      Result :: {ok, allow()}
              | {error, Error},
      Error :: no_allow
             | {invalid_allow, binary()}.
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_allow};
        Allows ->
            try
                L = lists:map(fun(Val) ->
                                      case ersip_method:parse(iolist_to_binary(Val)) of
                                          {ok, Method}->
                                              Method;
                                          {error, _} = Error ->
                                              throw(Error)
                                      end
                              end,
                              Allows),
                {ok, from_list(L)}
            catch
                throw:{error, _} = Error ->
                    {error, {invalid_allow, Error}}
            end
    end.

-spec build(HeaderName :: binary(), allow()) -> ersip_hdr:header().
build(HdrName, {allow, _} = Allow) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([assemble(Allow)], Hdr).

-spec assemble(allow()) ->  iolist().
assemble({allow, _} = Allow) ->
    ersip_iolist:join(<<", ">>,
                      [ersip_method:to_binary(Method)
                       || Method <- to_list(Allow)
                      ]).
'''