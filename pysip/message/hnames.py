from collections import namedtuple


HeaderKey = namedtuple('hdr_key', 'header')


def compact_form(header):
    return COMPACT_FORM_MAP.get(header.lower(), header.lower())


def print_form(header):
    return PRINT_FORM_MAP.get(make_key(header), header)


def known_header_form(header):
    return KNOWN_HEADER_KEY_MAP.get(make_key(header))


def make_key(header):
    if isinstance(header, HeaderKey):
        return header
    elif isinstance(header, (bytes, str)):
        return HeaderKey(compact_form(header))


FROM_HEADER = 'from'
TO_HEADER = 'to'
CALLID_HEADER = 'call-id'
CSEQ_HEADER = 'cseq'
MAXFORWARDS_HEADER = 'max-forwards'
VIA_HEADER = 'via'
ALLOW_HEADER = 'allow'
CONTACT_HEADER = 'contact'
PROXY_REQUIRE_HEADER = 'proxy-require'
RECORD_ROUTE_HEADER = 'record-route'
ROUTE_HEADER = 'route'
CONTENT_TYPE_HEADER = 'content-type'
UNSUPPORTED_HEADER = 'unsupported'
SUPPORTED_HEADER = 'supported'
REQUIRE_HEADER = 'require'
TOPMOST_VIA_HEADER = 'topmost_via'
DATE_HEADER = 'date'
EXPIRES_HEADER = 'expires'
MIN_EXPIRES_HEADER = 'min-expires'
ACCEPT_HEADER = 'accept'
ACCEPT_ENCODING_HEADER = 'accept-encoding'
ACCEPT_LANGUAGE_HEADER = 'accept-language'
WWW_AUTHENTICATE_HEADER = 'www-authenticate'
AUTHORIZATION_HEADER = 'authorization'
PROXY_AUTHENTICATE_HEADER = 'proxy-authenticate'
PROXY_AUTHORIZATION_HEADER = 'proxy-authorization'
CONTENT_LENGTH_HEADER = 'content-length'
CONTENT_ENCODING_HEADER = 'content-encoding'


PRINT_FORM_MAP = {HeaderKey('f'): 'From',
                  HeaderKey('t'): 'To',
                  HeaderKey('cseq'): 'CSeq',
                  HeaderKey('i'): 'Call-Id',
                  HeaderKey('v'): 'Via',
                  HeaderKey('max-forwards'): 'Max-Forwards',
                  HeaderKey('c'): 'Content-Type',
                  HeaderKey('route'): 'Route',
                  HeaderKey('record-route'): 'Record-Route',
                  HeaderKey('allow'): 'Allow',
                  HeaderKey('k'): 'Supported',
                  HeaderKey('unsupported'): 'Unsupported',
                  HeaderKey('require'): 'Require',
                  HeaderKey('proxy-require'): 'Proxy-Require',
                  HeaderKey('m'): 'Contact',
                  HeaderKey('expires'): 'Expires',
                  HeaderKey('min-expires'): 'Min-Expires'
                  }


COMPACT_FORM_MAP = {'accept-contact': 'a',
                    'allow-events': 'u',
                    CALLID_HEADER: 'i',
                    CONTACT_HEADER: 'm',
                    CONTENT_ENCODING_HEADER: 'e',
                    CONTENT_LENGTH_HEADER: 'l',
                    CONTENT_TYPE_HEADER: 'c',
                    'event': 'o',
                    FROM_HEADER: 'f',
                    'identity': 'y',
                    'refer-to': 'r',
                    'referred-by': 'b',
                    'reject-contact': 'j',
                    'request-disposition': 'd',
                    'session-expires': 'x',
                    'subject': 's',
                    SUPPORTED_HEADER: 'k',
                    TO_HEADER: 't',
                    VIA_HEADER: 'v'}

KNOWN_HEADER_KEY_MAP = {FROM_HEADER: HeaderKey('f'),
                        TO_HEADER: HeaderKey('t'),
                        CSEQ_HEADER: HeaderKey('cseq'),
                        CALLID_HEADER: HeaderKey('i'),
                        MAXFORWARDS_HEADER: HeaderKey('max-forwards'),
                        CONTENT_TYPE_HEADER: HeaderKey('c'),
                        ROUTE_HEADER: HeaderKey('route'),
                        RECORD_ROUTE_HEADER: HeaderKey('record-route'),
                        ALLOW_HEADER: HeaderKey('allow'),
                        SUPPORTED_HEADER: HeaderKey('k'),
                        UNSUPPORTED_HEADER: HeaderKey('unsupported'),
                        REQUIRE_HEADER: HeaderKey('require'),
                        PROXY_REQUIRE_HEADER: HeaderKey('proxy-require'),
                        CONTACT_HEADER: HeaderKey('m'),
                        EXPIRES_HEADER: HeaderKey('expires'),
                        MIN_EXPIRES_HEADER: HeaderKey('min-expires'),
                        DATE_HEADER: HeaderKey('date'),
                        TOPMOST_VIA_HEADER: HeaderKey('v')
                        }

ALL_KNOWN_HEADERS = [FROM_HEADER,
                     TO_HEADER,
                     CALLID_HEADER,
                     CSEQ_HEADER,
                     MAXFORWARDS_HEADER,
                     TOPMOST_VIA_HEADER,
                     CONTENT_TYPE_HEADER,
                     ALLOW_HEADER,
                     SUPPORTED_HEADER,
                     UNSUPPORTED_HEADER,
                     REQUIRE_HEADER,
                     PROXY_REQUIRE_HEADER,
                     ROUTE_HEADER,
                     RECORD_ROUTE_HEADER,
                     CONTACT_HEADER,
                     EXPIRES_HEADER,
                     MIN_EXPIRES_HEADER,
                     DATE_HEADER
                     ]


ALL_KNOWN_HEADERS_KEYS = list([make_key(h) for h in ALL_KNOWN_HEADERS])
