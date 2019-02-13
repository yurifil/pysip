import re
from pysip import PySIPException
from pysip.uri.uri import Uri
import pysip.message.parser_aux as parser_aux


class NameAddressError(PySIPException):
    pass


class NameAddress(object):
    PARSE_RX = re.compile(r'(".*?")\s*?<(.+?)>(.*)')
    BRACKETS_ADDRESS_RX = re.compile(r'\s*<(.*?)>(.*)')
    NO_DISPLAY_NAME_RX = re.compile(r'^(.*?)([,;].*)')

    def __init__(self, string=None):
        self.display_name = None
        self.uri = None
        self.rest = None
        if string is not None:
            if isinstance(string, str):
                self.display_name, self.uri, self.rest = NameAddress.parse(string)
            else:
                raise NameAddressError(f'Cannot parse string {string}: should be str but is of type {type(string)} '
                                       f'instead.')

    @staticmethod
    def parse(string):
        try:
            display_name, rest = NameAddress.parse_display_name(string)
            if isinstance(display_name, list) and len(display_name) == 1:
                display_name = display_name[0]
            address_match = NameAddress.BRACKETS_ADDRESS_RX.match(rest)
            if address_match:
                address, rest = address_match.group(1), address_match.group(2)
                uri = Uri(address)
                return display_name, uri, rest
            if not display_name:
                address_match = NameAddress.NO_DISPLAY_NAME_RX.match(rest)
                if address_match:
                    address, rest = address_match.group(1).strip(), address_match.group(2)
                    uri = Uri(address)
                    return display_name, uri, rest
                else:
                    uri = Uri(rest)
                    return display_name, uri, ''
        except NameAddressError as e:
            raise NameAddressError(f'Cannot parse {string}: {e}')
        except Exception as e:
            raise NameAddressError(f'Error occurred while parsing {string}: {e}')

    @staticmethod
    def parse_display_name(string):
        if not string:
            raise NameAddressError(f'Cannot parse display name: empty string')
        if string.startswith('"'):
            try:
                display_name, rest = parser_aux.quoted_string(string)
                return display_name, rest
            except parser_aux.ParserAUXError as e:
                raise NameAddressError(f'Cannot parse quoted display name: {e}')
        if parser_aux.is_token_char(string[0]):
            try:
                tokens, rest = parser_aux.token_list(string)
                if rest.startswith('<'):
                    return tokens, rest
                return '', string
            except parser_aux.ParserAUXError as e:
                raise NameAddressError(f'Cannot parse display name: {e}')
        if string[0] == '<':
            return '', string
        raise NameAddressError(f'Cannot parse display name {string}')

    @staticmethod
    def assemble_display_name(display_name):
        if isinstance(display_name, list):
            return ' '.join(display_name)
        else:
            return display_name

    @staticmethod
    def assemble(display_name, uri):
        assembled_display_name = NameAddress.assemble_display_name(display_name)
        assembled_uri = uri.uri
        if assembled_display_name:
            assembled_display_name += ' '
        return f'{assembled_display_name}<{assembled_uri}>'

'''
-spec assemble(display_name(), ersip_uri:uri()) -> iolist().
assemble(DisplayName, URI) ->
    DN = assemble_display_name(DisplayName),
    [DN,
     case ersip_iolist:is_empty(DN) of
         true ->
             <<"">>;
         false ->
             <<" ">>
     end,
     $<, ersip_uri:assemble(URI), $>
    ].

assemble_display_name({display_name, L}) when is_list(L) ->
    ersip_iolist:join(<<" ">>, L);
assemble_display_name({display_name, V}) when is_binary(V) ->
    V.
    

-spec parse_display_name(binary()) -> Result when
      Result :: {display_name(), Rest :: binary()}
              | error.
parse_display_name(<<>>) ->
    error;
parse_display_name(<<$", _/binary>> = Quoted) ->
    case ersip_parser_aux:quoted_string(Quoted) of
        {ok, Q, Rest} ->
            {{display_name, Q}, ersip_bin:trim_head_lws(Rest)};
        error ->
            error
    end;
parse_display_name(<<Char/utf8, _/binary>> = TL) when ?is_token_char(Char) ->
    case ersip_parser_aux:token_list(TL, lws) of
        {ok,  Tokens, Rest} ->
            case Rest of
                <<"<", _/binary>> ->
                    {{display_name,  Tokens}, Rest};
                _ ->
                    {{display_name, []}, TL}
            end;
        error ->
            {{display_name, []}, TL}
    end;
parse_display_name(<<"<", _/binary>> = Addr) ->
    {{display_name, []}, Addr};
parse_display_name(_) ->
    error.


-include("ersip_sip_abnf.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type display_name() :: {display_name, binary() | [binary()]}.

%%%===================================================================
%%% API
%%%===================================================================

%% [display-name] LAQUOT addr-spec RAQUOT
%% display-name   =  *(token LWS)/ quoted-string




%%%===================================================================
%%% Internal implementation
%%%===================================================================

%% display-name   =  *(token LWS)/ quoted-string
-spec parse_display_name(binary()) -> Result when
      Result :: {display_name(), Rest :: binary()}
              | error.


'''