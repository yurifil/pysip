from pysip.binary import to_string
from pysip import PySIPException
from pysip.message.hdr import Header
from pysip.message.hnames import CALLID_HEADER

WORD_CHARS = "\\-.!%*_+`'~()<>:\"/[]?{}"
AT = '@'


class CallIDError(PySIPException):
    pass


class CallIDHeader(Header):
    def __init__(self, callid):
        super().__init__(name=CALLID_HEADER)
        if isinstance(callid, (bytes, str)):
            self.add_value(self.parse(to_string(callid)))
        elif isinstance(callid, Header):
            self.add_value(self.parse_header(callid))
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

    def parse_header(self, header):
        if not header.values:
            raise CallIDError(f'Cannot parse header {header}: no values.')
        callid = ''
        for val in header.values:
            callid += val
        return self.parse(callid)

    def parse(self, callid):
        words = callid.split(AT)
        if not all([CallIDHeader.is_word(word) for word in words]):
            raise CallIDError(f'Invalid callid {callid}')
        return callid
