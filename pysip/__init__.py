
FINAL = 'final'
PROVISIONAL = 'provisional'
KNOWN_TRANSPORT = ('tcp', 'udp', 'tls', 'wss', 'ws')

TOKEN_CHARS = "-.!%*_+`'~"


def is_token_char(char):
    return char.isalnum() or char in TOKEN_CHARS


class PySIPException(Exception):
    pass


class HeaderError(PySIPException):
    def __init__(self, header=None, message=None):
        self.header = header
        self.message = message
