from pysip import PySIPException
from pysip.message.hnames import make_key
from pysip.message.hdr import Header
from pysip.message.method import Method

ITEM_TYPE = 'type'
ITEM_STATUS = 'status'
ITEM_REASON = 'reason'
ITEM_METHOD = 'method'
ITEM_RURI = 'ruri'
ITEM_BODY = 'body'

TYPE_REQUEST = 'request'
TYPE_RESPONSE = 'response'


class MessageError(PySIPException):
    pass


class MessageType(object):
    pass


class ResponseType(MessageType):
    def __init__(self, status=None, reason=None):
        self.name = TYPE_RESPONSE
        self.status = status
        self.reason = reason


class RequestType(MessageType):
    def __init__(self, method=None, ruri=None):
        self.name = TYPE_REQUEST
        self.method = method
        self.ruri = ruri


class Message(object):
    HEADERS_KEYS_ORDER = [make_key('via'),
                          make_key('to'),
                          make_key('from'),
                          make_key('call-id'),
                          make_key('cseq'),
                          make_key('max-forwards')]

    def __init__(self):
        self._type = None
        self._headers = dict()
        self.body = None
        self.source = None

    def __repr__(self):
        return f'Message: type={self._type} headers={self._headers} body={self.body}'

    @property
    def headers(self):
        return list(self._headers.values())

    def clear_header(self):
        self._headers = dict()

    def get(self, item):
        if item == ITEM_TYPE:
            return self._type.name
        elif item == ITEM_STATUS:
            if not isinstance(self._type, ResponseType):
                raise MessageError(f'Cannot get message status for {self}: message should be of type ResponseType')
            return self._type.status
        elif item == ITEM_REASON:
            if not isinstance(self._type, ResponseType):
                raise MessageError(f'Cannot get message reason for {self}: message should be of type ResponseType')
            return self._type.reason
        elif item == ITEM_METHOD:
            if not isinstance(self._type, RequestType):
                raise MessageError(f'Cannot get message method for {self}: message should be of type RequestType')
            return self._type.method
        elif item == ITEM_RURI:
            if not isinstance(self._type, RequestType):
                raise MessageError(f'Cannot get message ruri for {self}: message should be of type RequestType')
            return self._type.ruri
        elif item == ITEM_BODY:
            return self.body
        else:
            hdr = self._headers.get(make_key(item), Header(item))
            return hdr

    @property
    def reason(self):
        return self.get(ITEM_REASON)

    @reason.setter
    def reason(self, reason):
        if isinstance(self._type, ResponseType):
            self._type.reason = reason
        else:
            raise MessageError(f'Cannot set reason {reason} for message {self.serialize()}: message should be response,'
                               f'not {type(self._type)}.')

    @property
    def status(self):
        return self.get(ITEM_STATUS)

    @status.setter
    def status(self, status):
        if isinstance(self._type, ResponseType):
            self._type.status = status
        else:
            raise MessageError(f'Cannot set status {status} for message {self.serialize()}: message should be response,'
                               f'not {type(self._type)}.')

    @property
    def method(self):
        return self.get(ITEM_METHOD)

    @method.setter
    def method(self, method):
        if isinstance(self._type, RequestType):
            if isinstance(method, Method):
                self._type.method = method
            elif isinstance(method, str):
                self._type.method = Method(method)
            else:
                raise MessageError(f'Cannot set method {method} for message {self.serialize()}: method should be of '
                                   f'type Method or str, not {type(method)}')
        else:
            raise MessageError(f'Cannot set method {method} for message {self.serialize()}: message should be request, '
                               f'not {type(self._type)}')

    @property
    def ruri(self):
        return self.get(ITEM_RURI)

    @ruri.setter
    def ruri(self, ruri):
        if isinstance(self._type, RequestType):
            self._type.ruri = ruri
        else:
            raise MessageError(f'Cannot set ruri {ruri} for message {self.serialize()}: message should be request, '
                               f'not {type(self._type)}')

    @property
    def type(self):
        return self.get(ITEM_TYPE)

    @type.setter
    def type(self, value):
        if isinstance(value, (RequestType, ResponseType)):
            self._type = value
        else:
            raise MessageError(f'Cannot set type {value} for message {self.serialize()}: type should be a subclass of '
                               f'MessageType.')

    def add(self, name, value):
        if not make_key(name) in self._headers:
            self._headers[make_key(name)] = Header(name)
        if not isinstance(value, str):
            raise MessageError(f'Cannot add header {name}={value} to message {self.serialize()}: value should be '
                               f'string not ({type(value)}).')
        self._headers[make_key(name)].add_value(value.lstrip())

    def set(self, name, value):
        if name == ITEM_TYPE:
            self.type = value
        elif name == ITEM_BODY:
            self.body = value
        elif name == ITEM_REASON:
            self.reason = value
        elif name == ITEM_STATUS:
            self.status = value
        elif name == ITEM_METHOD:
            self.method = value
        elif name == ITEM_RURI:
            self.ruri = value
        else:
            key = make_key(name)
            hdr = Header(name)
            if isinstance(value, list):
                for v in value:
                    hdr.add_value(v)
            elif isinstance(value, str):
                hdr.add_value(value)
            self._headers[key] = hdr

    def set_header(self, header):
        self._headers[header.key] = header

    def serialize_first_line(self):
        if isinstance(self._type, RequestType):
            return f'{self._type.method} {self._type.ruri} SIP/2.0'
        elif isinstance(self._type, ResponseType):
            return f'SIP/2.0 {self._type.status} {self._type.reason}'

    def serialize(self):
        first_line = self.serialize_first_line()
        headers = self.serialize_headers()
        body = self.body
        if body is None:
            body = ''
        return f'{first_line}\r\n{headers}{body}'

    def serialize_headers_in_order(self, header_key_iterable):
        headers_list = []
        for key in header_key_iterable:
            if key in self._headers:
                headers_list.append(self._headers[key].serialize_to_string())
        return '\r\n'.join(headers_list)

    def serialize_headers(self):
        not_ordered_keys_set = set(self._headers.keys()).difference(Message.HEADERS_KEYS_ORDER)
        ordered_headers = self.serialize_headers_in_order(Message.HEADERS_KEYS_ORDER)
        not_ordered_headers = self.serialize_headers_in_order(not_ordered_keys_set)
        if not_ordered_headers:
            not_ordered_headers = '\r\n' + not_ordered_headers
        return ordered_headers + not_ordered_headers + '\r\n\r\n'

    def delete_header(self, header_name):
        self._headers.pop(make_key(header_name), None)
