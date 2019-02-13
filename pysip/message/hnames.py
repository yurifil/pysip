from collections import namedtuple

FROM_HEADER = 'from'
TO_HEADER = 'to'
CALLID_HEADER = 'call-id'
CSEQ_HEADER = 'cseq'
MAXFORWARDS_HEADER = 'max-forwards'
VIA_HEADER = 'via'
ALLOW_HEADER = 'Allow'
CONTACT_HEADER = 'Contact'

HeaderKey = namedtuple('hdr_key', 'header')


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
                    'call-id': 'i',
                    'contact': 'm',
                    'content-encoding': 'e',
                    'content-length': 'l',
                    'content-type': 'c',
                    'event': 'o',
                    'from': 'f',
                    'identity': 'y',
                    'refer-to': 'r',
                    'referred-by': 'b',
                    'reject-contact': 'j',
                    'request-disposition': 'd',
                    'session-expires': 'x',
                    'subject': 's',
                    'supported': 'k',
                    'to': 't',
                    'via': 'v'}

KNOWN_HEADER_KEY_MAP = {'from': HeaderKey('f'),
                        'to': HeaderKey('t'),
                        'cseq': HeaderKey('cseq'),
                        'callid': HeaderKey('i'),
                        'maxforwards': HeaderKey('max-forwards'),
                        'content_type': HeaderKey('c'),
                        'route': HeaderKey('route'),
                        'record_route': HeaderKey('record-route'),
                        'allow': HeaderKey('allow'),
                        'supported': HeaderKey('k'),
                        'unsupported': HeaderKey('unsupported'),
                        'require': HeaderKey('require'),
                        'proxy_require': HeaderKey('proxy-require'),
                        'contact': HeaderKey('m'),
                        'expires': HeaderKey('expires'),
                        'minexpires': HeaderKey('min-expires'),
                        'date': HeaderKey('date'),
                        'topmost_via': HeaderKey('v')
                        }

ALL_KNOWN_HEADERS = ['from',
                     'to',
                     'callid',
                     'cseq',
                     'maxforwards',
                     'topmost_via',
                     'content_type',
                     'allow',
                     'supported',
                     'unsupported',
                     'require',
                     'proxy_require',
                     'route',
                     'record_route',
                     'contact',
                     'expires',
                     'minexpires',
                     'date'
                     ]


def make_key(header):
    if isinstance(header, HeaderKey):
        return header
    elif isinstance(header, (bytes, str)):
        return HeaderKey(compact_form(header))


def compact_form(header):
    return COMPACT_FORM_MAP.get(header.lower(), header.lower())


def print_form(header):
    return PRINT_FORM_MAP.get(make_key(header), header)


def known_header_form(header):
    return KNOWN_HEADER_KEY_MAP.get(make_key(header))

'''




may_have_multiple_values({hdr_key, <<"accept-encoding">>}) ->
    true;
may_have_multiple_values({hdr_key, <<"accept-language">>}) ->
    true;
may_have_multiple_values({hdr_key, <<"www-authenticate">>}) ->
    true; %% WWW-Authenticate may have multiple values but they cannot be comma seprated.
may_have_multiple_values({hdr_key, <<"authorization">>}) ->
    true; %% Authorization may have multiple values but they cannot be comma seprated...
may_have_multiple_values({hdr_key, <<"proxy-authenticate">>}) ->
    true; %% Proxy-Authenticate may have multiple values but they cannot be comma seprated.
may_have_multiple_values({hdr_key, <<"proxy-authorization">>}) ->
    true; %% Proxy-Authorization may have multiple values but they cannot be comma seprated.
may_have_multiple_values(_) ->
    false.
'''