from pysip.uri.uri_parser import SIPUriParserBytes, SIPUriParserUnicode, ParseResult
from pysip.uri import URI_KEY_PARAMS, SIPUriError
from urllib.parse import unquote
from collections import OrderedDict


class Uri(object):

    def __init__(self, uri_str=None, **kwargs):
        if uri_str and kwargs:
            raise SIPUriError(f'Cannot initialize {self.__class__.__name__} instance: you should specify uri_str OR '
                              f'kwargs, not both. uri_str: {uri_str}, kwargs: {kwargs}')
        self._parsed_uri = ParseResult()
        self.parser_impl = None
        if uri_str:
            if isinstance(uri_str, bytes):
                self.parser_impl = SIPUriParserBytes()
            else:
                self.parser_impl = SIPUriParserUnicode()
            self._parsed_uri = self.parser_impl.parse(uri_str)

    @property
    def uri(self):
        scheme_str = self.scheme
        host_str = self.host
        user_key = self.user
        if not user_key:
            user_str = ''
        else:
            user_str = f'{user_key}@'
        if not self.port:
            port_str = ''
        else:
            port_str = f':{self.port}'
        if not self.params:
            params_str = ''
        else:
            params_str = ''
            for k, v in self.params.items():
                if v is True:
                    params_str = f';{k}{params_str}'
                elif v:
                    params_str = f';{k}={v}{params_str}'
                else:
                    params_str = f';{k}{params_str}'
        if not self.headers:
            headers_str = ''
        else:
            headers_list = list()
            for k, v in self.headers.items():
                if v:
                    headers_list.append(f'{k}={v}')
                else:
                    raise SIPUriError(f'Cannot represent headers {self.headers}: empty headers value for header {k}')
            headers_str = f'?{"&".join(headers_list)}'
        return f'{scheme_str}:{user_str}{host_str}{port_str}{params_str}{headers_str}'

    def __repr__(self):
        return self.uri

    def __eq__(self, other):
        # For two URIs to be equal, the user, password, host, and port
        # components must match.
        # Also some params and headers should be compared.
        if isinstance(other, Uri):
            return self.make_user_key() == other.make_user_key() and self.host == other.host \
                   and self.port == other.port and self.make_params_key() == other.make_params_key() \
                   and self.make_headers_key() == other.make_headers_key()
        return NotImplemented

    def make_headers_key(self):
        if self.headers:
            headers_key = dict()
            for k,v in self.headers.items():
                headers_key[unquote(k)] = unquote(v)
            return headers_key
        return self.headers

    @property
    def scheme(self):
        return self._parsed_uri.getscheme()

    @scheme.setter
    def scheme(self, val):
        self._parsed_uri.scheme = self.parser_impl.validate_scheme(val)

    @property
    def host(self):
        host = self._parsed_uri.gethost()
        if isinstance(host, str):
            return unquote(host)
        return host

    @host.setter
    def host(self, val):
        self._parsed_uri.host = self.parser_impl.validate_host(val)

    @property
    def port(self):
        return self._parsed_uri.getport()

    @port.setter
    def port(self, val):
        if val is not None:
            self._parsed_uri.port = self.parser_impl.validate_port(val)
        else:
            self._parsed_uri.port = val

    def get(self, part):
        if part not in self.parser_impl.SUPPORTED_PARTS:
            raise ValueError(f'Cannot get URI part {part}. Supported URI parts: {self.parser_impl.SUPPORTED_PARTS}')
        return self.__getattribute__(part)

    def assemble(self):
        return self._parsed_uri.geturi()

    def make_user_key(self):
        if self.user:
            return unquote(self.user)
        return self.user

    def make_params_key(self):
        params_key = OrderedDict()
        if self.params:
            for param in URI_KEY_PARAMS:
                if param in self.params:
                    params_key[param] = self.params[param]
        return params_key

    @property
    def user(self):
        return self.parser_impl.from_inner(self._parsed_uri.getuserinfo())

    @user.setter
    def user(self, val):
        self._parsed_uri.user = self.parser_impl.validate_user(val)

    @property
    def parsed_uri(self):
        return self._parsed_uri

    def clear_not_allowed_parts(self):
        raise NotImplementedError

    @property
    def params(self):
        return self._parsed_uri.getparams()

    @property
    def headers(self):
        return self._parsed_uri.getheaders()

    @staticmethod
    def make(**kwargs):
        if not kwargs:
            raise SIPUriError(f'Cannot make uri: no parts defined.')
        uri = Uri()
        for part, value in kwargs.items():
            if not uri.parser_impl:
                if (isinstance(part, bytes) or isinstance(part, str)) and isinstance(value, bytes):
                    uri.parser_impl = SIPUriParserBytes()
                elif isinstance(part, str) and isinstance(value, str):
                    uri.parser_impl = SIPUriParserUnicode()
                else:
                    raise SIPUriError(f'Cannot make uri from parts {kwargs}: unsupported type {part} ({type(part)}) or {value} ({type(value)}). Only bytes and str are supported.')
            if part not in uri.parser_impl.SUPPORTED_PARTS:
                raise SIPUriError(f'Cannot make uri from parts {kwargs}: part {part} is not in supported parts '
                                  f'list ({uri.parser_impl.SUPPORTED_PARTS}).')
            decoded_part = uri.parser_impl.from_inner(part)
            uri.__setattr__(decoded_part, value)
        return uri






