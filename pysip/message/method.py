from collections import namedtuple
from pysip import PySIPException
#from pysip.message.parser_aux import check_token

Method = namedtuple('method', 'method')

OPTIONS = Method(b'OPTIONS')
INVITE = Method(b'INVITE')
ACK = Method(b'ACK')
BYE = Method(b'BYE')
CANCEL = Method(b'CANCEL')
REGISTER = Method(b'REGISTER')


def options():
    return OPTIONS


def invite():
    return INVITE


def ack():
    return ACK


def bye():
    return bye()


def cancel():
    return CANCEL


def register():
    return REGISTER


def parse(binary):
    raise NotImplementedError


def to_binary(method):
    return method.method


def make(binary):
    if isinstance(binary, bytes):
        return parse(binary)
