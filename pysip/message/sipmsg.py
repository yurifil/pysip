from twisted.protocols.sip import MessagesParser, Message, Request, Response
from pysip.message.hnames import FROM_HEADER, TO_HEADER, CALLID_HEADER, CSEQ_HEADER, MAXFORWARDS_HEADER, VIA
from pysip import PySIPException
from pysip.message.status import reason_phrase


ALL = 'all'
# TODO: add type annotations here and there


class SipMsg(object):
    def __init__(self, message=None):
        if message is None:
            message = Message()
        if message is not None and not isinstance(message, (Message, Request, Response, SipMsg)):
            raise ValueError(f'SipMsg should be initialized with twisted.protocols.sip.Message object or None. '
                             f'Got {type(message)} instead.')
        if isinstance(message, SipMsg):
            self.__dict__.update(message.__dict__)
        else:
            self.message = message
        self._reason_val = None

    @property
    def ruri(self):
        return self.message.uri

    @ruri.setter
    def ruri(self, val):
        self.message.uri = val

    @property
    def status(self):
        return None

    @property
    def reason(self):
        return None

    @property
    def via(self):
        return self.__get_header(VIA, ALL)

    @via.setter
    def via(self, val):
        self.set_header(VIA, val)

    def add_header(self, name, value):
        self.message.addHeader(name, value)

    @property
    def headers(self):
        return self.message.headers

    @headers.setter
    def headers(self):
        raise NotImplementedError

    @property
    def type(self):
        return self.message.__class__.__name__

    def set_header(self, hdr, value):
        pass

    def __get_header(self, hdr, index=0):
        if index == ALL:
            header = self.message.headers.get(hdr)
        elif isinstance(index, int):
            header = self.message.headers.get(hdr)[index]
        else:
            raise ValueError(f'Unsupported index value: {index}')
        return header

    @property
    def from_value(self):
        return self.__get_header(FROM_HEADER)

    @from_value.setter
    def from_value(self, val):
        self.set_header(FROM_HEADER, val)

    @property
    def to_value(self):
        return self.__get_header(TO_HEADER)

    @to_value.setter
    def to_value(self, val):
        self.set_header(TO_HEADER, val)

    @property
    def callid(self):
        return self.__get_header(CALLID_HEADER)

    @callid.setter
    def callid(self, val):
        self.set_header(CALLID_HEADER, val)

    @property
    def cseq(self):
        return self.__get_header(CSEQ_HEADER)

    @cseq.setter
    def cseq(self, val):
        self.set_header(CSEQ_HEADER, val)

    @property
    def maxforwards(self):
        return self.__get_header(MAXFORWARDS_HEADER)

    @maxforwards.setter
    def maxforwards(self, val):
        self.set_header(MAXFORWARDS_HEADER, val)

    @property
    def topmost_via(self):
        pass

    @topmost_via.setter
    def topmost_via(self, val):
        raise NotImplementedError

    @property
    def reason(self):
        return self._reason_val

    @reason.setter
    def reason(self, val):
        self._reason_val = val

    def remove_header(self):
        pass

    def has_body(self):
        pass

    def remove_body(self):
        pass

    def get_raw_header(self, hdr_name):
        pass

    def set_raw_header(self, raw_hdr):
        pass

    @property
    def source(self):
        pass

    @source.setter
    def source(self, src):
        raise NotImplementedError

    @property
    def user_data(self):
        pass

    @user_data.setter
    def user_data(self, data):
        raise NotImplementedError

    def clear_user_data(self):
        raise NotImplementedError

    def reply(self, reply):
        raise NotImplementedError

    def serialize(self):
        raise NotImplementedError

    def new_reply(self, status, reason, method):
        raise NotImplementedError

    def clear_headers(self):
        self.message.headers.clear()

    @property
    def method(self):
        if isinstance(self.message, Request):
            return self.message.method
        else:
            raise ValueError('Method property is not defined for response message.')

    @method.setter
    def method(self, method):
        self.message.method = method


class SIPMessageParseError(PySIPException):
    pass


# TODO: do not append to list, this is ugly.
# TODO: check stuff after twisted parsed the message - it is too forgiving.
def parse(raw_msg):
    l = list()
    parser = MessagesParser(messageReceivedCallback=l.append)
    parser.dataReceived(raw_msg)
    parser.dataDone()
    if len(l) < 1:
        raise SIPMessageParseError(f'Could not parse SIP message:\n{raw_msg}\n')
    return SipMsg(message=l[0])


class Reply(SipMsg):
    def __init__(self, message=None, options=None):
        super().__init__(message=message)
        if options is not None:
            self.__dict__.update(options.__dict__)


class ReplyOptions(object):
    def __init__(self, status, reason=None, to_tag=None):
        self.status = status
        self._reason_val = reason
        self.to_tag = to_tag

    @property
    def reason(self):
        if self._reason_val is None:
            return reason_phrase(self.status)
        else:
            return self._reason_val

    @reason.setter
    def reason(self, reason):
        self._reason_val = reason

