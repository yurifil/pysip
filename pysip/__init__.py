
FINAL = 'final'
PROVISIONAL = 'provisional'
KNOWN_TRANSPORT = ('tcp', 'udp', 'tls', 'wss', 'ws')


class PySIPException(Exception):
    pass


class HeaderError(PySIPException):
    def __init__(self, header=None, message=None):
        self.header = header
        self.message = message
