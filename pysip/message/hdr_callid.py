from pysip.binary import to_string
from pysip import PySIPException
from pysip.message.hdr import Header, BaseSipHeader

WORD_CHARS = "\\-.!%*_+`'~()<>:\"/[]?{}"
AT = '@'


class CallIDError(PySIPException):
    pass


class CallIDHeader(BaseSipHeader):
    def __init__(self, callid):
        self.call_id = None
        if isinstance(callid, (bytes, str)):
            self.call_id = self.parse_callid(to_string(callid))
        elif isinstance(callid, Header):
            self.call_id = self.parse_header(callid)
        else:
            raise CallIDError(f'Cannot parse callid {callid}: callid should be of type str or '
                              f'{Header.__class__.__name__}, but it is of type {type(callid)}')

    @staticmethod
    def is_word_char(char):
        return char.isalnum() or char in WORD_CHARS

    @staticmethod
    def is_word(string):
        if not string:
            return False
        for sym in string:
            if sym.isalnum() or sym in WORD_CHARS:
                continue
            else:
                return False
        return True

    @staticmethod
    def parse_header(header):
        if not header.values:
            raise CallIDError(f'Cannot parse header {header}: no values.')
        callid = ''
        for val in header.values:
            callid += val
        return CallIDHeader.parse(callid)

    @staticmethod
    def parse_callid(callid):
        words = callid.split(AT)
        if not all([CallIDHeader.is_word(word) for word in words]):
            raise CallIDError(f'Invalid callid {callid}')
        return callid

    @staticmethod
    def parse(callid):
        return CallIDHeader(callid)

    def assemble(self):
        return self.call_id

    def build(self, header_name):
        hdr = Header(header_name)
        hdr.add_value(self.assemble())
        return hdr
