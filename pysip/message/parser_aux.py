from pysip import is_token_char, PySIPException
from pysip.uri.uri_parser import SIPUriParserUnicode
from urllib.parse import unquote
import re


QUOTED_STRING_RX = re.compile(r'"(.+)"')
SPLIT_PATTERN_RX = re.compile(r'\s+')

PARSER = SIPUriParserUnicode()

START_STATE = 'start'
ESCAPED_STATE = 'escaped'
RAW_STATE = 'raw'


class ParserAUXError(PySIPException):
    pass


'''
compile_pattern(lws) ->
    binary:compile_pattern([<<" ">>, <<"\t">>]).

token_list(Binary, SEP) ->
    CompiledPattern = compile_pattern(SEP),
    token_list_impl(Binary, [], CompiledPattern).

token_list_impl(Binary, Acc, CompPattern) ->
    case binary:split(Binary, CompPattern) of
        [<<>>, Rest] ->
            token_list_impl(Rest, Acc, CompPattern);
        [T, Rest] ->
            case check_token(T) of
                true ->
                    token_list_impl(Rest, [T | Acc], CompPattern);
                false ->
                    token_list_impl_process_rest(Acc, Binary)
            end;
        [T] ->
            case check_token(T) of
                true ->
                    {ok, lists:reverse([T | Acc]), <<>>};
                false ->
                    token_list_impl_process_rest(Acc, Binary)
            end
    end.

token_list_impl_process_rest(Acc, Binary) ->
    {Acc1, Rest1} =
        case find_token_end(Binary, 0) of
            0 ->
                {Acc, Binary};
            N ->
                <<LastToken:N/binary, Rest0/binary>> = Binary,
                {[LastToken|Acc], Rest0}
        end,
    case Acc1 of
        [] ->
            error;
        _ ->
            {ok, lists:reverse(Acc1), Rest1}
    end.
'''


def token_list_process_rest(tokens, string):
    token_end = find_token_end(string)
    if token_end == 0:
        acc, rest = tokens, string
    else:
        acc, rest = tokens.copy(), string[token_end:]
        acc.append(string[:token_end])
    if len(acc) == 0:
        raise ParserAUXError(f'Cannot process token list rest {tokens}, {string}')
    return acc, rest


def token_list_impl(string, acc):
    splitted_string = SPLIT_PATTERN_RX.split(string, maxsplit=1)
    if len(splitted_string) == 2:
        if splitted_string[0] == '':
            return token_list_impl(splitted_string[1], acc)
        else:
            if check_token(splitted_string[0]):
                acc.append(splitted_string[0])
                return token_list_impl(splitted_string[1], acc)
            else:
                return token_list_process_rest(acc, string)
    elif len(splitted_string) == 1:
        if check_token(splitted_string[0]):
            return acc
        else:
            return token_list_process_rest(acc, string)


def token_list(string):
    return token_list_impl(string, [])


def parse_slash(string):
    string = string.strip()
    if string.startswith('/'):
        return '/', string[1:].strip()


def parse_token(string):
    # TODO: check arithmetics here
    end = find_token_end(string)
    if end == 0:
        raise ParserAUXError(f'Cannot parse token from string {string}: not a token.')
    return string[:end], string[end:]


def check_token(token):
    for sym in token:
        if not is_token_char(sym):
            return False
    return True


def find_token_end(string):
    pos = 0
    for sym in string:
        if is_token_char(sym):
            pos += 1
        else:
            break
    return pos


def parse_separator(string, separator):
    if string.startswith(separator):
        return separator, string[len(separator):]


def get_quoted_string_rest(string, state):
    if string[0] == '"' and state == START_STATE:
        return get_quoted_string_rest(string[1:], state=RAW_STATE)
    elif state == START_STATE:
        raise ParserAUXError(f'Invalid START_STATE found for {string}')
    if string[0] == '"' and state == RAW_STATE:
        return string[1:]
    if string[0] == '\\' and state == RAW_STATE:
        return get_quoted_string_rest(string[1:], state=ESCAPED_STATE)
    if state == RAW_STATE:
        return get_quoted_string_rest(string[1:], state=RAW_STATE)


def quoted_string(string):
    if not string:
        raise ParserAUXError('Not a quoted string: empty string')
    '''
        url_unquoted_string = unquote(string)
        if QUOTED_STRING_RX.match(url_unquoted_string):
            return True
        return False
        '''
    try:
        urlunquoted_string = unquote(string)
    except SyntaxError as e:
        raise ParserAUXError(f'Cannot parse quoted string {string}: could not urlunquote:\n{e}')
    trimmed_string = urlunquoted_string.strip()
    try:
        rest = get_quoted_string_rest(trimmed_string, state=START_STATE)
        return trimmed_string[:len(trimmed_string)-len(rest)], trimmed_string[len(trimmed_string)-len(rest):]
    except ParserAUXError as e:
        raise ParserAUXError(f'Cannot parse quoted string {string}: {e}')
    except Exception as e:
        raise e

'''
%% @private
-spec quoted_string_impl(binary(), State) -> {ok, Rest :: binary()} | error when
      State :: start
             | raw
             | escaped.

quoted_string_impl(<<>>, _) ->
    error;
quoted_string_impl(<<"\"", R/binary>>, start) ->
    quoted_string_impl(R, raw);
quoted_string_impl(_, start) ->
    error;
quoted_string_impl(<<"\"", R/binary>>, raw) ->
    {ok, R};
quoted_string_impl(<<"\\", R/binary>>, raw) ->
    quoted_string_impl(R, escaped);
quoted_string_impl(B, raw) ->
    case utf8_len(B) of
        {ok, Len} ->
            RestLen = byte_size(B) - Len,
            quoted_string_impl(binary:part(B, Len, RestLen), raw);
        error ->
            error
    end;
quoted_string_impl(<<Byte:8, R/binary>>, escaped) when Byte =< 16#7F ->
    quoted_string_impl(R, raw).

-spec quoted_string(Quoted) -> {ok, Quoted, Rest} | error when
      Quoted :: binary(),
      Quoted :: binary(),
      Rest   :: binary().
quoted_string(Quoted) ->
    Trimmed = ersip_bin:trim_head_lws(Quoted),
    TrimmedLen = byte_size(Trimmed),
    case quoted_string_impl(Trimmed, start) of
        {ok, Rest} ->
            Len = TrimmedLen - byte_size(Rest),
            {ok, binary:part(Trimmed, 0, Len), Rest};
        error ->
            error
    end.
'''


def parse_gen_param_value(v):
    if v is None:
        return None
    try:
        q, r = quoted_string(v)
        return v
    except ParserAUXError:
        if PARSER.is_valid_token(v):
            return v
        else:
            try:
                host = PARSER.validate_host(v)
                return v
            except ValueError as e:
                raise ParserAUXError(f'Cannot parse param {v}: {e}')


def parse_params(param_string, separator):
    params_list = list()
    raw_pairs_list = param_string.split(separator)
    for raw_pair in raw_pairs_list:
        key_value_pair = raw_pair.split('=')
        if len(key_value_pair) > 1:
            k, v = key_value_pair[0].strip(), key_value_pair[1].strip()
        else:
            k, v = key_value_pair[0].strip(), None
        if not PARSER.is_valid_token(k):
            raise ParserAUXError(f'Invalid parameter name {k}: not a token.')
        param = parse_gen_param_value(v)
        params_list.append((k, v))
    return params_list


def parse_all(string, parser_fun_list):
    result = list()
    string_rest = string
    for parser_fun in parser_fun_list:
        res_temp, string_rest = parser_fun(string_rest)
        result.append(result)
    return result



'''
%% @doc Apply series of parsers:
-spec parse_all(binary(), [ParserFun]) -> ParseAllResult when
      ParserFun      :: fun((binary()) -> parse_result()),
      ParseAllResult :: {ok, [ParseResult], Rest :: binary()}
                      | {error, term()},
      ParseResult    :: term().
parse_all(Binary, Parsers) ->
    parse_all_impl(Binary, Parsers, []).

-spec parse_all_impl(binary(), [ParserFun], Acc) -> ParseAllResult when
      ParserFun      :: fun((binary()) -> parse_result()),
      ParseAllResult :: {ok, [ParseResult], Rest :: binary()}
                      | {error, term()},
      ParseResult    :: term(),
      Acc            :: list().
parse_all_impl(Binary, [], Acc) ->
    {ok, lists:reverse(Acc), Binary};
parse_all_impl(Binary, [F | FRest], Acc) ->
    case F(Binary) of
        {ok, ParseResult, BinRest} ->
            parse_all_impl(BinRest, FRest, [ParseResult | Acc]);
        {error, Error} ->
            {error, Error}
    end.



%% gen-value      =  token / host / quoted-string
-spec parse_gen_param_value(binary()) -> parse_result(gen_param_value()).
parse_gen_param_value(<<>>) ->
    {ok, <<>>, <<>>};
parse_gen_param_value(Bin) ->
    case quoted_string(Bin) of
        {ok, Val, Rest} ->
            {ok, Val, Rest};
        error ->
            case parse_token(Bin) of
                {ok, _, _} = R ->
                    R;
                _ ->
                    case ersip_host:parse(Bin) of
                        {ok, _Host, Rest} ->
                            RestPos = byte_size(Bin) - byte_size(Rest),
                            <<HostBin:RestPos/binary, _/binary>> = Bin,
                            {ok, HostBin, Rest};
                        _ ->
                            {error, {inval_gen_param_value, Bin}}
                    end
            end
    end.

-spec do_parse_params(binary(), char(), gen_param_list()) -> parse_result(gen_param_list()).
do_parse_params(Bin, Sep, Acc) ->
    case parse_gen_param(Bin) of
        {ok, Val, Rest0} ->
            Rest1 = ersip_bin:trim_lws(Rest0),
            case parse_sep(Sep, Rest1) of
                {ok, _, Rest2} ->
                    do_parse_params(ersip_bin:trim_lws(Rest2), Sep, [Val | Acc]);
                {error, _} ->
                    {ok, lists:reverse([Val | Acc]), Rest0}
            end;
        {error, _} = Error ->
            Error
    end.

%% generic-param 1*(Sep generic-param)
-spec parse_params(char(), binary()) -> parse_result(gen_param_list()).
parse_params(Sep, Bin) ->
    do_parse_params(ersip_bin:trim_head_lws(Bin), Sep, []).

%% generic-param  =  token [ EQUAL gen-value ]
-spec parse_gen_param(binary()) -> parse_result(gen_param()).
parse_gen_param(Bin) ->
    case parse_token(Bin) of
        {ok, Key, Rest0} ->
            Rest1 = ersip_bin:trim_head_lws(Rest0),
            case parse_sep($=, Rest1) of
                {ok, _, Rest2} ->
                    Rest3 = ersip_bin:trim_head_lws(Rest2),
                    case parse_gen_param_value(Rest3) of
                        {ok, Value, Rest4} ->
                            {ok, {Key, Value}, Rest4};
                        {error, Reason} ->
                            {error, {invalid_param_value, Reason}}
                    end;
                {error, _} ->
                    {ok, {Key, <<>>}, Rest0}
            end;
        {error, Reason} ->
            {error, {invalid_param, Reason}}
    end.

-spec parse_sep(char(), binary()) -> parse_result(char()).
parse_sep(Sep, <<Sep/utf8, R/binary>>) ->
    {ok, Sep, R};
parse_sep(Sep, Bin) ->
    {error, {no_separator, Sep, Bin}}.

-include("ersip_sip_abnf.hrl").

-export([quoted_string/1,
         token_list/2,
         check_token/1,
         parse_all/2,
         parse_token/1,
         parse_lws/1,
         trim_lws/1,
         parse_slash/1,
         parse_sep/2,
         parse_non_neg_int/1,
         parse_kvps/3,
         parse_params/2,
         parse_gen_param_value/1
        ]).

-export_type([parse_result/0,
              parse_result/1,
              parser_fun/0,
              separator/0,
              gen_param_list/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type parse_result(T) :: parse_result(T, term()).
-type parse_result(T, Err) :: {ok, ParseResult :: T, Rest :: binary()}
                            | {error, Err}.
-type parse_result() :: parse_result(term()).

-type parser_fun()           :: fun((binary()) -> parse_result()).
-type separator()            :: lws.
-type parse_kvps_validator() ::
        fun((Key :: binary(),
             MayBeValue :: novalue | binary())
            -> {ok, {Key :: term(), Value :: term()}}
                   | {error, term()}
                   | skip).
-type gen_param()      :: {Key ::binary(), gen_param_value()}.
-type gen_param_list() :: [gen_param()].
-type gen_param_value() :: binary().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Parse quoted string (with unquoute).
%% DQUOTE *(qdtext / quoted-pair ) DQUOTE
%% qdtext         =  LWS / %x21 / %x23-5B / %x5D-7E
%%                        / UTF8-NONASCII




%% @doc Check binary is token
%% token       =  1*(alphanum / "-" / "." / "!" / "%" / "*"
%%                   / "_" / "+" / "`" / "'" / "~" )
-spec check_token(binary()) -> boolean().
check_token(Bin) ->
    check_token(Bin, start).


-spec parse_lws(binary()) ->  parse_result({lws, pos_integer()}).
parse_lws(Bin) ->
    Trimmed = ersip_bin:trim_head_lws(Bin),
    case byte_size(Bin) - byte_size(Trimmed) of
        0 ->
            {error,  {lws_expected, Bin}};
        N ->
            {ok, {lws, N}, Trimmed}
    end.

%% SLASH   =  SWS "/" SWS ; slash
-spec parse_slash(binary()) -> ersip_parser_aux:parse_result().
parse_slash(Binary) ->
    SEPParser = fun(Bin) -> parse_sep($/, Bin) end,
    Parsers = [fun trim_lws/1,
               SEPParser,
               fun trim_lws/1
              ],
    case ersip_parser_aux:parse_all(Binary, Parsers) of
        {ok, [_, _, _], Rest} ->
            {ok, slash, Rest};
        {error, _} = Error ->
            Error
    end.


-spec trim_lws(binary()) ->  {ok, {lws, pos_integer()}, Rest :: binary()}.
trim_lws(Bin) ->
    Trimmed = ersip_bin:trim_head_lws(Bin),
    N = byte_size(Bin) - byte_size(Trimmed),
    {ok, {lws, N}, Trimmed}.

-spec parse_non_neg_int(binary()) -> parse_result(non_neg_integer(), {invalid_integer, binary()}).
parse_non_neg_int(Bin) ->
    parse_non_neg_int_impl(Bin, start, 0).

%% @doc Parse key-value pairs sepeated with Sep.
%% Validator may:
%% - transform key-value to another key/value pair
%% - skip key-value pair
%% - return error on pair
-spec parse_kvps(Validator, Sep, binary()) -> parse_result([{Key, Value} | Key]) when
      Key   :: binary(),
      Value :: binary(),
      Sep   :: binary(),
      Validator :: parse_kvps_validator().
parse_kvps(_, _, <<>>) ->
    {ok, [], <<>>};
parse_kvps(Validator, Sep, Bin) ->
    KVPs = binary:split(Bin, Sep, [global]),
    SplittedPairs =
        lists:map(fun(KVP) -> binary:split(KVP, <<"=">>) end,
                  KVPs),
    try
        ParseResult =
            lists:filtermap(parse_kvps_make_validator_func(Validator),
                            SplittedPairs),
        {ok, ParseResult, <<>>}
    catch
        throw:Error ->
            Error
    end.


%%%===================================================================
%%% Internal implementation
%%%===================================================================



%% @private
%% @doc get length of the first UTF8 character in binary
-spec utf8_len(binary()) -> {ok, Len :: 1..6} | error.
utf8_len(<<ASCII:8, _/binary>>) when ASCII =< 16#7F ->
    {ok, 1};
utf8_len(<<UTF8_1:8, _:8, _/binary>>)
  when UTF8_1 >= 16#C0 andalso UTF8_1 =< 16#DF  ->
    {ok, 2};
utf8_len(<<UTF8_2:8, _:16, _/binary>>)
  when UTF8_2 >= 16#E0 andalso UTF8_2 =< 16#EF ->
    {ok, 3};
utf8_len(<<UTF8_3:8, _:24, _/binary>>)
  when UTF8_3 >= 16#F0 andalso UTF8_3 =< 16#F7 ->
    {ok, 4};
utf8_len(<<UTF8_4:8, _:32, _/binary>>)
  when UTF8_4 >= 16#F8 andalso UTF8_4 =< 16#FB ->
    {ok, 5};
utf8_len(<<UTF8_5:8, _:40, _/binary>>)
  when UTF8_5 >= 16#FC andalso UTF8_5 =< 16#FD ->
    {ok, 6};
utf8_len(_) ->
    error.




%% @private
-spec check_token(binary(), start | rest) -> boolean().
check_token(<<>>, start) ->
    false;
check_token(<<>>, rest) ->
    true;
check_token(<<Char/utf8, R/binary>>, _) when ?is_token_char(Char) ->
    check_token(R, rest);
check_token(_, _) ->
    false.




-spec parse_non_neg_int_impl(binary(), State , Acc) -> parse_result(non_neg_integer()) when
      State :: start | rest,
      Acc :: non_neg_integer().
parse_non_neg_int_impl(<<Char/utf8, _/binary>> = Bin, start, Acc) when Char >= $0 andalso Char =< $9 ->
    parse_non_neg_int_impl(Bin, rest, Acc);
parse_non_neg_int_impl(Bin, start, _) ->
    {error, {invalid_integer, Bin}};
parse_non_neg_int_impl(<<Char/utf8, R/binary>>, rest, Acc) when Char >= $0 andalso Char =< $9 ->
    parse_non_neg_int_impl(R, rest, Acc * 10 + Char - $0);
parse_non_neg_int_impl(Rest, rest, Acc) ->
    {ok, Acc, Rest}.

-spec parse_kvps_make_validator_func(parse_kvps_validator()) -> KVPsMapFun when
      KVPsMapFun :: fun((list()) -> {term(), term()}).
parse_kvps_make_validator_func(Validator) ->
    fun([Key, Value]) ->
            TKey = ersip_bin:trim_lws(Key),
            TVal = ersip_bin:trim_lws(Value),
            case Validator(TKey, TVal) of
                {ok, {K, V}} ->
                    {true, {K, V}};
                skip ->
                    false;
                {error, _} = Error ->
                    throw(Error)
            end;
       ([Key]) ->
            TKey = ersip_bin:trim_lws(Key),
            case Validator(TKey, novalue) of
                {ok, {K, V}} ->
                    {true, {K, V}};
                skip ->
                    false;
                {error, _} = Error ->
                    throw(Error)
            end
    end.




'''