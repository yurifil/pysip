from pysip.message.hdr import Header
from pysip.message.hparams import HParams, HParamNotFound, HParamsError
from pysip.uri.uri import Uri
from pysip.message.hnames import CONTACT_HEADER
from pysip.message.nameaddr import NameAddress, NameAddressError
from pysip.message.parser_aux import parse_params, check_token, ParserAUXError
from pysip import PySIPException
from pysip.message.qvalue import QValue, QValueError
from pysip.uri.uri_parser import SIPUriParserUnicode


EXPIRES = 'expires'
Q = 'q'


def is_integer(string):
    try:
        int(string)
        return True
    except ValueError:
        return False


class ContactHeaderError(PySIPException):
    pass


class ContactHeader(object):
    def __init__(self, contact_string=None):
        '''
        :param display_name: str
        :param uri: Uri()
        :param hparams: HParams()
        '''
        self.display_name = None
        self.uri = Uri()
        self.uri.parser_impl = SIPUriParserUnicode()
        self.params = HParams()
        self.rest = None
        if contact_string is not None:
            self.display_name, self.uri, self.params, self.rest = self.parse_hdr(contact_string)

    def parse_contact_params(self, contact_params):
        print(f'parse_contact_params({contact_params})')
        if contact_params.startswith(';'):
            return self.do_parse_contact_params(contact_params[1:])
        return HParams(), ''

    @staticmethod
    def do_parse_contact_params(string):
        print(f'do_parse_contact_params({string})')
        try:
            hparams = HParams()
            params_list = parse_params(string, ';')
            print(f'params_list: {params_list}')
            for k, v in params_list:
                if k.lower() in (EXPIRES, Q):
                    hparams.set(k.lower(), ContactHeader.parse_param(k.lower(), v), k, v)
                elif check_token(k.lower()):
                    hparams.set_raw(k, v)
                else:
                    raise ContactHeaderError(f'Key {k}={v} is not a token.')
            return hparams, ''
        except (ParserAUXError, HParamsError, ContactHeaderError) as e:
            raise ContactHeaderError(f'Cannot parse contact header {string}: {e}')

    def parse_hdr(self, string):
        try:
            print(f'parse_hdr({string})')
            nameaddr = NameAddress(string)
            display_name = nameaddr.display_name
            uri = nameaddr.uri
        except NameAddressError as e:
            raise ContactHeaderError(f'Cannot parse contact header {string} nameaddress part: {e}')
        print(f'display_name: {display_name}, uri: {uri}, rest: {nameaddr.rest}')
        contact_params, rest = self.parse_contact_params(nameaddr.rest)
        return display_name, uri, contact_params, rest

    def get_expires(self, default=None):
        expires = self.params.find(EXPIRES)
        if isinstance(expires, HParamNotFound):
            return default
        return expires

    def set_expires(self, expires_val):
        if is_integer(expires_val):
            self.params.set(EXPIRES, expires_val, EXPIRES, expires_val)
        else:
            raise ContactHeaderError(f'Cannot set expires parameter {expires_val}: not an integer')

    def get_qvalue(self, default=None):
        qval =  self.params.find('q')
        if isinstance(qval, HParamNotFound):
            return default
        return qval

    def set_qvalue(self, qvalue):
        self.params.set(Q, qvalue, Q, qvalue)

    @staticmethod
    def parse_param(param_name, param_value):
        if param_name == EXPIRES:
            try:
                return int(param_value)
            except ValueError:
                raise ContactHeaderError(f'Cannot parse expires param: value {param_value} is not int.')
        if param_name == Q:
            try:
                return QValue(param_value)
            except QValueError as e:
                raise ContactHeaderError(f'Cannot parse q param: {e}')

    def __eq__(self, other):
        if isinstance(other, ContactHeader):
            print(f'self.display_name {self.display_name} == other.display_name {other.display_name} and self.uri '
                  f'{self.uri} == other.uri {other.uri} and self.params {self.params} == other.params {other.params}')
            return self.display_name == other.display_name and self.uri == other.uri and self.params == other.params
        return NotImplemented

    def assemble(self):
        assembled_hparams = self.params.assemble()
        if assembled_hparams:
            assembled_hparams = f';{assembled_hparams}'
        return f'{NameAddress.assemble(self.display_name, self.uri)}{assembled_hparams}'

    def set_param(self, name, value):
        if name.lower() in (EXPIRES, Q):
            try:
                parsed_value = self.parse_param(name, value)
                self.params.set(name.lower(), parsed_value, name, value)
            except ContactHeaderError as e:
                raise ContactHeaderError(f'Cannot set param {name}={value}: {e}')
        elif check_token(name.lower()):
            self.params.set_raw(name, value)
        else:
            raise ContactHeaderError(f'Cannot set param {name}={value}: invalid param.')

    def get_param(self, name):
        return self.params.find_raw(name)


'''
-spec set_param(Name :: binary(), PValue :: binary(), contact()) -> contact().
set_param(PName, PValue, #contact{hparams = HParams} = Contact)
        when is_binary(PName), is_binary(PValue) ->
    case set_hparam(PName, PValue, HParams) of
        {ok, NewHParam} ->
            Contact#contact{hparams = NewHParam};
        {error, Reason} ->
            error(Reason)
    end.

set_hparam(PName, PValue, HParams) ->
    LowerName = ersip_bin:to_lower(PName),
    case param_name_to_atom(LowerName) of
        {ok, ParsedName} ->
            case parse_param(ParsedName, PValue) of
                {ok, ParsedValue} ->
                    {ok, ersip_hparams:set(ParsedName, ParsedValue, PName, PValue, HParams)};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error;
        not_found ->
            {ok, ersip_hparams:set_raw(PName, PValue, HParams)}
    end

param_name_to_atom(<<"expires">>) -> {ok, expires};
param_name_to_atom(<<"q">>)       -> {ok, q};
param_name_to_atom(X) when is_binary(X) ->
    case ersip_parser_aux:check_token(X) of
        true -> not_found;
        false -> {error, {invalid_param, X}}
    end.
    


-spec assemble(contact()) -> iolist().
assemble(#contact{} = Contact) ->
    #contact{display_name = DN,
           uri = URI,
           hparams = HParams
          } = Contact,
    HParamsIO0 = ersip_hparams:assemble(HParams),
    HParamsIO =
        case ersip_iolist:is_empty(HParamsIO0) of
            true -> [];
            false -> [$; | HParamsIO0]
        end,
    [ersip_nameaddr:assemble(DN, URI), HParamsIO].


-spec parse_param(known_param(), binary()) -> {ok, Value} | {error, Err} when
      Value :: {expires, integer()}
             | {q, ersip_qvalue:qvalue()},
      Err   :: {invalid_expires, binary()}
             | {invalid_qvalue, binary()}.
parse_param(expires, Value) ->
    try
        {ok, binary_to_integer(Value)}
    catch
        error:badarg ->
            {error, {invalid_expires, Value}}
    end;
parse_param(q, Value) ->
    case ersip_qvalue:parse(Value) of
        {ok, _} = Ok -> Ok;
        {error, Reason} ->
            {error, {invalid_qvalue, Reason}}
    end.





-spec parse_contact_params(binary()) -> ersip_parser_aux:parse_result(ersip_hparams:hparams()).
parse_contact_params(<<$;, Bin/binary>>) ->
    do_parse_contact_params(Bin);
parse_contact_params(Bin) ->
    {ok, ersip_hparams:new(), Bin}.

-spec do_parse_contact_params(binary()) -> ersip_parser_aux:parse_result(ersip_hparams:hparams()).
do_parse_contact_params(Bin) ->
    case ersip_parser_aux:parse_params($;, Bin) of
        {ok, PList, Rest} ->
            try
                HParams =
                    lists:foldl(fun({Key, Value}, HParams) ->
                                        case set_hparam(Key, Value, HParams) of
                                            {ok, NewHParam} ->
                                                NewHParam;
                                            {error, _} = Error ->
                                                throw(Error)
                                        end
                                end,
                                ersip_hparams:new(),
                                PList),
                {ok, HParams, Rest}
            catch
                throw:{error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec parse(binary()) -> parse_result().
parse(Bin) ->
    case parse_hdr(Bin) of
        {ok, Contact, <<>>} ->
            {ok, Contact};
        {ok, _, _} ->
            {error, {invalid_contact, Bin}};
        {error, _} = Err ->
            Err
    end.

-spec parse_hdr(binary()) -> ersip_parser_aux:parse_result(contact()).
parse_hdr(Bin) ->
    Parsers = [fun ersip_nameaddr:parse/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_contact_params/1
              ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [{DisplayName, URI}, _, HParams], Rest} ->
            Contact = #contact{display_name = DisplayName,
                               uri          = URI,
                               hparams      = HParams},
            {ok, Contact, Rest};
        {error, Reason} ->
            {error, {invalid_contact, Reason}}
    end.


-spec make(binary()) -> contact().
make(Bin) when is_binary(Bin) ->
    case ersip_hdr_contact:parse(Bin) of
        {ok, Contact} ->
            Contact;
        {error, Reason} ->
            error(Reason)
    end.


-spec set_qvalue(ersip_qvalue:qvalue(), contact()) -> contact().
set_qvalue({qvalue, _} = QVal, #contact{hparams = HParams} = Contact) ->
    NewHParams = ersip_hparams:set(q, QVal, <<"q">>, ersip_qvalue:assemble(QVal), HParams),
    Contact#contact{hparams = NewHParams}.


-export([uri/1,
         expires/2,
         set_expires/2,
         qvalue/2,
         set_qvalue/2,
         param/2,
         set_param/3,
         make/1,
         parse/1,
         parse_hdr/1,
         assemble/1
        ]).
-export_type([contact/0,
              contact_param/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(contact, {display_name  :: undefined | ersip_nameaddr:display_name(),
                  uri           :: ersip_uri:uri(),
                  hparams       :: ersip_hparams:hparams()
                 }).
-type contact() :: #contact{}.

-type contact_param() :: {qvalue, ersip_qvalue:qvalue()}
                       | {expires, expires()}
                       | {binary(), binary()}.
-type expires() :: non_neg_integer().

-type parse_result() :: {ok, contact()}
                      | {error, {invalid_contact, term()}}.
-type known_param() :: q | expires.

%%%===================================================================
%%% API
%%%===================================================================

-spec uri(contact()) -> ersip_uri:uri().
uri(#contact{uri = URI}) ->
    URI.



-spec param(Name :: binary(), contact()) -> {ok, Value :: binary()} | not_found.
param(Name, #contact{hparams = HParams}) when is_binary(Name) ->
    ersip_hparams:find_raw(Name, HParams).






%%%===================================================================
%%% Internal Implementation
%%%===================================================================

-spec param_name_to_atom(binary()) -> {ok, known_param()} | not_found | {error, {invalid_param, binary()}}.








-spec set_hparam(Name :: binary(), PValue :: binary(), ersip_hparams:hparams()) -> Result when
      Result :: {ok, ersip_hparams:hparams()}
              | {error, term()}.


'''