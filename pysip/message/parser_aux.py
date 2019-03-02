from pysip import is_token_char, PySIPException
from pysip.uri.uri_parser import SIPUriParserUnicode
from pysip.uri import HostParseError
from urllib.parse import unquote
import re


QUOTED_STRING_RX = re.compile(r'"(.+)"')
SPLIT_PATTERN_RX = re.compile(r'\s+')

INTEGER_RX = re.compile(r'^(\d+)(\D*)')

PARSER = SIPUriParserUnicode()

START_STATE = 'start'
ESCAPED_STATE = 'escaped'
RAW_STATE = 'raw'


class ParserAUXError(PySIPException):
    pass


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
    else:
        raise ParserAUXError(f"Cannot parse slash from {string}: doesn't start with slash")


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
    if state == ESCAPED_STATE: # TODO: check that next value is <= 7F
        return get_quoted_string_rest(string[1:], state=RAW_STATE)
    if state == RAW_STATE:
        return get_quoted_string_rest(string[1:], state=RAW_STATE)


def quoted_string(string):
    if not string:
        raise ParserAUXError('Not a quoted string: empty string')
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


def parse_non_negative_integer(number):
    if isinstance(number, str):
        m = INTEGER_RX.match(number)
        if m:
            return int(m.group(1)), m.group(2)
        else:
            return None, number
    elif isinstance(number, int):
        if number >= 0:
            return number
        else:
            raise ParserAUXError(f'Cannot parse non negative integer from {number}')
    else:
        try:
            int_number = int(number)
            if int_number >= 0:
                return int_number
            else:
                raise ParserAUXError(f'Cannot parse non negative integer from {number}')
        except Exception as e:
            raise ParserAUXError(f'Cannot parse non negative integer from {number}: {e}')


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
            except HostParseError as e:
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
