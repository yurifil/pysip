from collections import namedtuple
from pysip.binary import to_lower

FROM_HEADER = 'from'
TO_HEADER = 'to'
CALLID_HEADER = 'call-id'
CSEQ_HEADER = 'cseq'
MAXFORWARDS_HEADER = 'max-forwards'
VIA_HEADER = 'via'
ALLOW_HEADER = 'Allow'
CONTACT_HEADER = 'Contact'


HeaderKey = namedtuple('hdr_key', 'header')


PRINT_FORM_MAP = {HeaderKey(b'f'): b'From',
                  HeaderKey(b't'): b'To',
                  HeaderKey(b'cseq'): b'CSeq',
                  HeaderKey(b'i'): b'Call-Id',
                  HeaderKey(b'v'): b'Via',
                  HeaderKey(b'max-forwards'): b'Max-Forwards',
                  HeaderKey(b'c'): b'Content-Type',
                  HeaderKey(b'route'): b'Route',
                  HeaderKey(b'record-route'): b'Record-Route',
                  HeaderKey(b'allow'): b'Allow',
                  HeaderKey(b'k'): b'Supported',
                  HeaderKey(b'unsupported'): b'Unsupported',
                  HeaderKey(b'require'): b'Require',
                  HeaderKey(b'proxy-require'): b'Proxy-Require',
                  HeaderKey(b'm'): b'Contact',
                  HeaderKey(b'expires'): b'Expires',
                  HeaderKey(b'min-expires'): b'Min-Expires'
                  }


COMPACT_FORM_MAP = {b'accept-contact': b'a',
                    b'allow-events': b'u',
                    b'call-id': b'i',
                    b'contact': b'm',
                    b'content-encoding': b'e',
                    b'content-length': b'l',
                    b'content-type': b'c',
                    b'event': b'o',
                    b'from': b'f',
                    b'identity': b'y',
                    b'refer-to': b'r',
                    b'referred-by': b'b',
                    b'reject-contact': b'j',
                    b'request-disposition': b'd',
                    b'session-expires': b'x',
                    b'subject': b's',
                    b'supported': b'k',
                    b'to': b't',
                    b'via': b'v'}

KNOWN_HEADER_KEY_MAP = {'from': HeaderKey(b'f'),
                        'to': HeaderKey(b't'),
                        'cseq': HeaderKey(b'cseq'),
                        'callid': HeaderKey(b'i'),
                        'maxforwards': HeaderKey(b'max-forwards'),
                        'content_type': HeaderKey(b'c'),
                        'route': HeaderKey(b'route'),
                        'record_route': HeaderKey(b'record-route'),
                        'allow': HeaderKey(b'allow'),
                        'supported': HeaderKey(b'k'),
                        'unsupported': HeaderKey(b'unsupported'),
                        'require': HeaderKey(b'require'),
                        'proxy_require': HeaderKey(b'proxy-require'),
                        'contact': HeaderKey(b'm'),
                        'expires': HeaderKey(b'expires'),
                        'minexpires': HeaderKey(b'min-expires'),
                        'date': HeaderKey(b'date'),
                        'topmost_via': HeaderKey(b'v')
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
    return COMPACT_FORM_MAP.get(to_lower(header), to_lower(header))


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