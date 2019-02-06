from pysip.binary import to_integer
from pysip import KNOWN_TRANSPORT, TOKEN_CHARS
from uritools.encoding import uridecode
import re
import string
from urllib.parse import unquote, unquote_to_bytes
from pysip.uri import PARAM_TRANSPORT, PARAM_MADDR, PARAM_USER, PARAM_USER_IP, PARAM_USER_PHONE, PARAM_LR, PARAM_TTL, \
    _ip_literal, _ipv4_address, URIParseError, UserParseError, HostParseError, PortParseError, ParamParseError, \
    SipUriTransportError

# TODO: host is always stored as str. Switch to inner representation


class SIPUriParser(object):
    PARTS_SCHEME = 'scheme'
    PARTS_HOST = 'host'
    PARTS_PORT = 'port'
    PARTS_USER = 'user'
    PARTS_PARAMS = 'params'
    PARTS_HEADERS = 'headers'
    SUPPORTED_PARTS = (PARTS_HOST, PARTS_PORT, PARTS_USER, PARTS_SCHEME, PARTS_PARAMS, PARTS_HEADERS)

    TYPE = None
    SUPPORTED_SCHEMES = ('sip', 'sips')

    def _unquote(self, quoted_str):
        raise NotImplementedError

    def from_inner(self, char):
        raise NotImplementedError

    def to_inner(self, char):
        raise NotImplementedError

    def _normalize(self, char):
        raise NotImplementedError

    def _decode_int(self, val):
        return to_integer(val)

    def _is_valid_user(self, user):
        symbols = iter(user)
        if not user:
            return False
        for int_sym in symbols:
            sym = self._normalize(int_sym)
            # TODO: this ought to be really slow. Fix this.
            if self.from_inner(sym).isalnum() or sym in self.MARK or sym in self.USER_UNRESERVED:
                continue
            elif sym == self.PERCENT_SIGN:
                try:
                    next1 = self._normalize(next(symbols))
                    next2 = self._normalize(next(symbols))
                except StopIteration:
                    return False
                if self.from_inner(next1) in string.hexdigits and self.from_inner(next2) in string.hexdigits:
                    continue
            else:
                return False
        return True

    def _is_valid_passwd(self, passwd):
        symbols = iter(passwd)
        for int_sym in symbols:
            sym = self._normalize(int_sym)
            # TODO: this ought to be really slow. Fix this.
            if self.from_inner(sym).isalnum() or sym in self.MARK or sym in self.PASSWD_UNRESERVED:
                print(f'Sym {sym} isalnum: {self.from_inner(sym).isalnum()}')
                print('Continuing')
                continue
            elif sym == self.PERCENT_SIGN:
                try:
                    next1 = self._normalize(next(symbols))
                    next2 = self._normalize(next(symbols))
                except StopIteration:
                    return False
                if self.from_inner(next1) in string.hexdigits and self.from_inner(next2) in string.hexdigits:
                    continue
            else:
                return False
        return True

    def _check_userinfo(self, userinfo):
        splitted = userinfo.split(self.COLON)
        print(f'Splitted userinfo: {splitted}')
        is_valid = self._is_valid_user(splitted[0])
        print(f'Is valid user: {is_valid}')
        if len(splitted) > 1:
            is_valid = is_valid and self._is_valid_passwd(splitted[1])
        return is_valid

    def validate_user(self, user):
        if not self._check_userinfo(user):
            raise UserParseError(f'Invalid userinfo: {user}')
        return user

    def parse_user(self, authority):
        print(f'Authority: {authority}')
        if authority is None:
            return None
        else:
            userinfo, present, _ = authority.rpartition(self.AT)
            print(f'Userinfo: {userinfo}. Present: {present}. Rest: {_}')
            if not userinfo:
                if present:
                    raise UserParseError(f'Invalid (empty) userinfo in authority {authority}')
                return None
            return self.validate_user(userinfo)

    @staticmethod
    def is_valid_port(port):
        if 0 < port <= 65535:
            return True
        return False

    def validate_port(self, port):
        if not isinstance(port, int):
            decoded_port = self._decode_int(port)
        else:
            decoded_port = port
        if not self.is_valid_port(decoded_port):
            raise PortParseError(f'Cannot parse port {port}: invalid value.')
        else:
            return decoded_port

    def parse_port(self, authority):
        if authority is None:
            return None
        _, present, port = authority.rpartition(self.COLON)
        if present and not port.strip().lstrip(self.DIGITS):
            return self.validate_port(port)
        else:
            return None

    # TODO: do this without recursion!
    def domainlabel_valid(self, bytes_list):
        if len(bytes_list) == 0:
            return False
        if len(bytes_list) == 1 and self.from_inner(self._normalize(bytes_list[0])).isalnum():
            return True
        if len(bytes_list) == 2 and self._normalize(bytes_list[0]) == self.DOT and self.from_inner(self._normalize(bytes_list[1])).isalnum():
            return True
        if self._normalize(bytes_list[0]) == self.DOT and self.from_inner(self._normalize(bytes_list[1])).isalnum():
            return self.domainlabel_valid(bytes_list[2:])
        if self.from_inner(self._normalize(bytes_list[0])).isalnum() and self._normalize(bytes_list[1]) == self.DOT:
            return self.domainlabel_valid(bytes_list[1:])
        if bytes_list[0] == self.DASH:
            return self.domainlabel_valid(bytes_list[1:])
        if self.from_inner(self._normalize(bytes_list[0])).isalnum():
            return self.domainlabel_valid(bytes_list[1:])
        return False

    # TODO: do this without recursion!
    def toplabel_valid(self, bytes_list):
        if len(bytes_list) == 0:
            return False
        if len(bytes_list) == 1 and self.from_inner(self._normalize(bytes_list[0])).isalnum():
            return True
        if self.from_inner(self._normalize(bytes_list[0])).isalnum() and self._normalize(bytes_list[1]) == self.DOT:
            return self.domainlabel_valid(bytes_list[1:])
        if self._normalize(bytes_list[0]) == self.DASH:
            return self.toplabel_valid(bytes_list[1:])
        if self.from_inner(self._normalize(bytes_list[0])).isalnum():
            return self.toplabel_valid(bytes_list[1:])
        return False

    def _check_host(self, host):
        reversed_symbols = list(reversed(list(iter(host))))
        int_sym = reversed_symbols[0]
        sym = self._normalize(int_sym)
        if sym == self.DOT:
            next_sym = self._normalize(next(reversed_symbols))
            if self.from_inner(next_sym).isalnum():
                return self.toplabel_valid(list(reversed_symbols[1:]))
        elif self.from_inner(sym).isalnum():
            return self.toplabel_valid(list(reversed_symbols))
        else:
            return False
        return True

    def validate_host(self, host):
        if host.startswith(self.LBRACKET) and host.endswith(self.RBRACKET):
            return _ip_literal(host[1:-1])
        elif host.startswith(self.LBRACKET) or host.endswith(self.RBRACKET):
            raise ValueError(f'Invalid host {host}')
        ret_host = _ipv4_address(host)
        if not ret_host:
            if self._check_host(host):
                ret_host = uridecode(host, 'utf-8', 'strict').lower()
            else:
                raise HostParseError(f'Cannot parse host {host}')
        return ret_host

    def parse_host(self, authority):
        if authority is None:
            return None
        _, _, hostinfo = authority.rpartition(self.AT)
        host, _, port = hostinfo.rpartition(self.COLON)
        if port.strip().lstrip(self.DIGITS):
            return self.validate_host(hostinfo)
        else:
            return self.validate_host(host)

    @staticmethod
    def _is_valid_scheme(scheme):
        if scheme is None:
            return False
        return True

    def validate_scheme(self, scheme):
        if not self._is_valid_scheme(scheme):
            raise URIParseError('Invalid scheme')
        return scheme

    def parse_scheme(self, scheme):
        return self.validate_scheme(scheme)

    def _split_uri(self, uri_str):
        scheme = None
        hostport = None
        params = None
        headers = None
        (scheme, authority, path, query, fragment) = self.RX.match(uri_str).groups()
        print(f'scheme: {scheme}, authority: {authority}, path: {path}, query: {query}, fragment: {fragment}')
        if self.SEMICOLON in authority:
            hostport, present, rest = authority.partition(self.SEMICOLON)
            params = rest
        else:
            hostport = authority
        if query:
            headers = query
        print(f'scheme: {scheme}, hostport: {hostport}, params: {params}, headers: {headers}')
        return scheme, hostport, params, headers

    def _validate_transport(self, transport):
        if self.from_inner(transport).lower() not in KNOWN_TRANSPORT and not self.is_valid_token(transport):
            raise SipUriTransportError(f'Invalid transport: {transport}')
        return self.to_inner(self.from_inner(transport).lower())

    def is_valid_token(self, token):
        if not token:
            return False
        symbols = iter(token)
        for int_sym in symbols:
            sym = self._normalize(int_sym)
            # TODO: this ought to be really slow. Fix this.
            if self.from_inner(sym).isalnum() or sym in self.TOKEN_CHAR:
                continue
            else:
                return False
        return True

    def _validate_user_param(self, user):
        if user.lower() in (PARAM_USER_IP, PARAM_USER_PHONE):
            return user.lower()
        elif self.is_valid_token(user):
            return user
        else:
            raise ParamParseError(f'Cannot parse user param user={user}')

    def _validate_param(self, param, value):
        try:
            param_lower = self.from_inner(param).lower()
            if param_lower == PARAM_TRANSPORT:
                value = self._validate_transport(value)
            elif param_lower == PARAM_MADDR:
                value = self.validate_host(value)
            elif param_lower == PARAM_USER:
                value = self._validate_user_param(value)
            elif param_lower == PARAM_LR:
                value = True
            elif param_lower == PARAM_TTL:
                value = self._decode_int(value)
                if value <= 0 or value >= 255:
                    raise ParamParseError(f'Wrong TTL value ({value}).')
            return param, value
        except Exception as e:
            raise ParamParseError(f'Cannot parse {param}={value}: {e}')

    def parse_params(self, params_str):
        if not params_str:
            return None
        params = dict()
        for pair in params_str.split(self.SEMICOLON):
            splitted_param_pair = pair.split(self.EQUAL)
            param = splitted_param_pair[0]
            if len(splitted_param_pair) > 1:
                value = splitted_param_pair[1]
            else:
                value = None
            param, value = self._validate_param(param, value)
            params[param] = value
        print(f'Parsed params: {params}')
        return params

    def _validate_header(self, header, value):
        return self._unquote(header), self._unquote(value)

    def parse_headers(self, headers_str):
        if not headers_str:
            return None
        headers = dict()
        for pair in headers_str.split(self.AMP):
            splitted_header_pair = pair.split(self.EQUAL)
            header = splitted_header_pair[0]
            if len(splitted_header_pair) > 1:
                value = splitted_header_pair[1]
            else:
                value = None
            header, value = self._validate_header(header, value)
            headers[header] = value
        print(f'Parsed headers: {headers}')
        return headers

    def parse(self, uri_str):
        result = ParseResult(encoding=self.ENCODING)
        scheme, hostport, params, headers = self._split_uri(uri_str)
        result.scheme = self.parse_scheme(scheme)
        result.host = self.parse_host(hostport)
        result.port = self.parse_port(hostport)
        result.user = self.parse_user(hostport)
        result.params = self.parse_params(params)
        result.headers = self.parse_headers(headers)
        return result


class SIPUriParserUnicode(SIPUriParser):
    TYPE = str

    ENCODING = 'utf-8'

    PARAMS_RX = re.compile(r'(.*)?;.*')
    RX = re.compile(r"""
    (?:([A-Za-z][A-Za-z0-9+.-]*):)?  # scheme (RFC 3986 3.1)
    (?:/*/*([^/?#]*))?                 # authority
    ([^?#]*)                         # path
    (?:\?([^#]*))?                   # query
    (?:\#(.*))?                      # fragment
    """, flags=re.VERBOSE)

    # RFC 3986 2.2 gen-delims
    COLON, SEMICOLON, SLASH, QUEST, HASH, LBRACKET, RBRACKET, AT = (
        u':', u';', u'/', u'?', u'#', u'[', u']', u'@'
    )

    DASH = u'-'

    EQUAL = u'='

    # RFC 3986 3.3 dot-segments
    DOT, DOTDOT = u'.', u'..'

    EMPTY, EQ = u'', u'='

    DIGITS = u'0123456789'

    PERCENT_SIGN = u'%'

    AMP = u'&'

    USER_UNRESERVED = u'&=+$,;?/'

    PASSWD_UNRESERVED = u'&=+$,'

    TOKEN_CHAR = TOKEN_CHARS

    MARK = u"-_.!~*'()"

    def from_inner(self, char):
        return char

    def _normalize(self, char):
        return char

    def _unquote(self, quoted_str):
        return unquote(quoted_str, self.ENCODING)

    def to_inner(self, part_name):
        return part_name


class SIPUriParserBytes(SIPUriParser):
    TYPE = bytes

    ENCODING = 'utf-8'

    RX = re.compile(br"""
    (?:([A-Za-z][A-Za-z0-9+.-]*):)?  # scheme (RFC 3986 3.1)
    (?:/*/*([^/?#]*))?                 # authority
    ([^?#]*)                         # path
    (?:\?([^#]*))?                   # query
    (?:\#(.*))?                      # fragment
    """, flags=re.VERBOSE)

    # RFC 3986 2.2 gen-delims
    COLON, SEMICOLON, SLASH, QUEST, HASH, LBRACKET, RBRACKET, AT = (
        b':', b';', b'/', b'?', b'#', b'[', b']', b'@'
    )

    DASH = b'-'

    EQUAL = b'='

    # RFC 3986 3.3 dot-segments
    DOT, DOTDOT = b'.', b'..'

    EMPTY, EQ = b'', b'='

    DIGITS = b'0123456789'

    PERCENT_SIGN = b'%'

    AMP = b'&'

    USER_UNRESERVED = b'&=+$,;?/'

    PASSWD_UNRESERVED = b'&=+$,'

    TOKEN_CHAR = TOKEN_CHARS.encode(SIPUriParserUnicode.ENCODING)

    MARK = b"-_.!~*'()"

    def from_inner(self, char):
        if isinstance(char, (bytearray, bytes)):
            return char.decode(self.ENCODING)
        return char

    def _normalize(self, char):
        if isinstance(char, int):
            return bytes([char])
        return char

    def _unquote(self, quoted_str):
        return unquote_to_bytes(quoted_str)

    def to_inner(self, part_name):
        if isinstance(part_name, str):
            return part_name.encode(self.ENCODING)
        return part_name


class ParseResult(object):
    def __init__(self, encoding='utf-8'):
        self.scheme = None
        self.host = None
        self.port = None
        self.user = None
        self.params = None
        self.headers = None
        self._encoding = encoding
        self._errors = 'strict'

    def getscheme(self):
        scheme = self.scheme
        if scheme is None:
            return None
        elif isinstance(scheme, bytes):
            return scheme.decode('ascii').lower()
        else:
            return scheme.lower()

    def getuserinfo(self):
        userinfo = self.user
        if userinfo is None:
            return None
        elif isinstance(userinfo, bytes):
            return userinfo.decode(self._encoding)
        else:
            return userinfo

    def gethost(self):
        return self.host

    def getport(self):
        port = self.port
        if port:
            return int(port)
        else:
            return None

    def getparams(self):
        params = self.params
        if not params:
            return None
        ret_dict = dict()
        for k, v in params.items():
            key = k
            value = v
            if isinstance(k, bytes):
                key = k.decode(self._encoding)
            if isinstance(v, bytes):
                value = v.decode(self._encoding)
            ret_dict[key.lower()] = value
        return ret_dict

    def getheaders(self):
        headers = self.headers
        if not headers:
            return None
        ret_dict = dict()
        for k, v in headers.items():
            key = k
            value = v
            if isinstance(k, bytes):
                key = k.decode(self._encoding)
            if isinstance(v, bytes):
                value = v.decode(self._encoding)
            ret_dict[key.lower()] = value
        return ret_dict


