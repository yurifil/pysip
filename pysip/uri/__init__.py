import ipaddress

URI_KEY_PARAMS = ['user', 'transport', 'ttl', 'method']

PARAM_TRANSPORT = 'transport'
PARAM_MADDR = 'maddr'
PARAM_USER = 'user'
PARAM_USER_IP = 'ip'
PARAM_USER_PHONE = 'phone'
PARAM_LR = 'lr'
PARAM_TTL = 'ttl'
PARAM_RECEIVED = 'received'
PARAM_BRANCH = 'branch'
PARAM_RPORT = 'rport'


def _ip_literal(address):
    if isinstance(address, bytes):
        address = address.decode('ascii')
    if address.startswith(u'v'):
        raise ValueError('address mechanism not supported')
    return ipaddress.IPv6Address(address)


def _ipv4_address(address):
    try:
        if isinstance(address, bytes):
            return ipaddress.IPv4Address(address.decode('ascii'))
        else:
            return ipaddress.IPv4Address(address)
    except ValueError:
        return None


class URIParseError(ValueError):
    pass


class UserParseError(URIParseError):
    pass


class HostParseError(URIParseError):
    pass


class PortParseError(URIParseError):
    pass


class ParamParseError(URIParseError):
    pass


class SipUriTransportError(URIParseError):
    pass


class SIPUriError(Exception):
    pass
