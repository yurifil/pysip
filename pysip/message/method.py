from pysip import PySIPException


class MethodError(PySIPException):
    pass


class Method(object):
    TOKEN_CHARS = "-.!%*_+`'~"

    def __init__(self, method):
        self.method = method
        if not Method._is_valid_token(self.method):
            raise MethodError(f'Cannot parse method {self.method}: not a valid token.')

    def __repr__(self):
        return self.method

    def __eq__(self, other):
        if isinstance(other, Method):
            return self.method == other.method
        return NotImplemented

    @staticmethod
    def _is_valid_token(string):
        if not string:
            return False
        for sym in string:
            if sym.isalnum() or sym in Method.TOKEN_CHARS:
                continue
            else:
                return False
        return True


OPTIONS = Method('OPTIONS')
INVITE = Method('INVITE')
ACK = Method('ACK')
BYE = Method('BYE')
CANCEL = Method('CANCEL')
REGISTER = Method('REGISTER')


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




'''
from collections import namedtuple
from pysip.binary import to_string
from pysip import PySIPException

#Method = namedtuple('method', 'method')

TOKEN_CHARS = "-.!%*_+`'~"


class MethodError(PySIPException):
    pass


class Method(object):
    def __init__(self, method):
        self.method = to_string(method)
        if not self._is_valid_token(self.method):
            raise MethodError(f'Cannot parse method "{self.method}": not a valid token.')

    def __repr__(self):
        return self.method

    def __eq__(self, other):
        if isinstance(other, Method):
            return self.method == other.method
        return NotImplemented

    @staticmethod
    def _is_valid_token(string):
        if not string:
            return False
        for sym in string:
            if sym.isalnum() or sym in TOKEN_CHARS:
                continue
            else:
                return False
        return True


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
'''