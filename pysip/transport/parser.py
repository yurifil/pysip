from pysip import PySIPException

from pysip.message.message import Message, ResponseType, RequestType
from pysip.message.method import Method, MethodError
from pysip.transport.buffer import new_dgram, new
from pysip.transport import MoreDataRequired, MAX_MESSAGE_LENGTH_UNLIMITED

import re


STATE_FIRST_LINE = 'first_line'
STATE_HEADERS = 'headers'
STATE_BODY = 'body'

DEFAULT_OPTIONS = {'buffer': dict(), 'max_message_len': 8192}


class TransportParserError(PySIPException):
    pass


class Data(object):
    def __init__(self, options=None, buf=None, state=STATE_FIRST_LINE, message=None, acc=None, content_length=None,
                 start_pos=0):
        if options is None:
            self.options = dict()
        else:
            self.options = options
        self.buffer = buf
        self.state = state
        if message is None:
            self.message = Message()
        else:
            self.message = message
        if acc is None:
            self.acc = list()
        else:
            self.acc = acc
        self.content_length = content_length
        self.start_pos = start_pos

    def __repr__(self):
        return f'Data(options={self.options}, buffer={self.buffer}, state={self.state}, message={self.message}, acc={self.acc}, content_length={self.content_length})'

    def add(self, string):
        self.buffer.add(string)


class Parser(object):
    STATUS_LINE_RX = re.compile(r'SIP/2\.0 (\d{3}) (.+?)$')

    @staticmethod
    def new(options=None):
        if options is None:
            options = dict()
        if isinstance(options, dict):
            return Data(options={**DEFAULT_OPTIONS, **options}, buf=new(options))
        raise TransportParserError(f'Cannot initialize datagram with options {options}: options should be dict not '
                                   f'{type(options)}')

    @staticmethod
    def new_dgram(dgram_str):
        return Data(buf=new_dgram(dgram_str), options={'max_message_len': MAX_MESSAGE_LENGTH_UNLIMITED})

    @staticmethod
    def parse(data):
        if data.state == STATE_FIRST_LINE:
            return Parser.parse_first_line(data)
        elif data.state == STATE_HEADERS:
            return Parser.parse_headers(data)
        elif data.state == STATE_BODY:
            message = Parser.parse_body(data)
            return message

    @staticmethod
    def parse_first_line(data):
        line = data.buffer.read_till_crlf()
        if isinstance(line, MoreDataRequired):
            return Parser.more_data_required(data)
        elif isinstance(line, str):
            return Parser.parse_first_line_string(line, data)

    @staticmethod
    def parse_first_line_string(first_line, data):
        if first_line.startswith('SIP/'):
            return Parser.parse_status_line(first_line, data)
        else:
            return Parser.parse_request_line(first_line, data)

    @staticmethod
    def parse_status_line(first_line, data):
        match_res = Parser.STATUS_LINE_RX.match(first_line)
        if match_res is not None:
            status_code, reason_phrase = int(match_res.group(1)), match_res.group(2)
            if 100 <= status_code <= 699:
                data.message.type = ResponseType()
                data.message.status = status_code
                data.message.reason = reason_phrase
                data.state = STATE_HEADERS
                return Parser.parse(data)
            else:
                raise TransportParserError(f'Bad status code {status_code} in status line "{first_line}"')
        else:
            raise TransportParserError(f'Bad status line {first_line}')

    @staticmethod
    def parse_request_line(first_line, data):
        try:
            method_str, ruri, sip_2_str = first_line.split(' ')
            if sip_2_str != 'SIP/2.0':
                raise TransportParserError(f'Bad request line "{first_line}": invalid SIP version')
        except Exception as e:
            raise TransportParserError(f'Bad request line "{first_line}": {e}')
        try:
            method = Method(method_str)
        except MethodError as e:
            raise TransportParserError(f'Bad request line "{first_line}": {e}')
        data.message.type = RequestType()
        data.message.method = method
        data.message.ruri = ruri
        data.state = STATE_HEADERS
        return Parser.parse(data)

    @staticmethod
    def parse_headers(data):
        #print(f'Parser.parse_headers({data})')
        if not data.acc:
            line = data.buffer.read_till_crlf()
            if line == '':
                raise TransportParserError(f'Cannot parse message {data}: no headers')
            if isinstance(line, MoreDataRequired):
                return Parser.more_data_required(data)
            data.acc.append(line)
            return Parser.parse_headers(data)
        else:
            line = data.buffer.read_till_crlf()
            if isinstance(line, MoreDataRequired):
                return Parser.more_data_required(data)
            if line.startswith(' ') or line.startswith('\t'):
                data.acc.append(line)
                return Parser.parse_headers(data)
            if line == '':
                Parser.add_header(data.acc, data)
                data.acc = list()
                data.state = STATE_BODY
                return Parser.parse(data)
            else:
                Parser.add_header(data.acc, data)
                data.acc = list()
                data.acc.append(line)
                return Parser.parse_headers(data)

    @staticmethod
    def parse_body(data):
        if data.content_length is None:
            try:
                [content_length] = data.message.get('content-length').values
                content_length = int(content_length)
                if content_length >= 0:
                    data.content_length = content_length
                    return Parser.parse_body(data)
                else:
                    raise ValueError
            except (IndexError, ValueError):
                if data.buffer.has_eof():
                    data.content_length = data.buffer.length
                    return Parser.parse_body(data)
                else:
                    raise TransportParserError(f'Bad message: invalid content length.')
        else:
            content = data.buffer.read(data.content_length)
            if isinstance(content, MoreDataRequired):
                if data.buffer.has_eof():
                    raise TransportParserError(f'Truncated message')
                else:
                    return Parser.more_data_required(data)
            else:
                stream_position = data.buffer.pos
                message_length = stream_position - data.start_pos
                data.message.body = content
                data.content_length = None
                data.state = STATE_FIRST_LINE
                data.start_pos = stream_position
                return Parser.message_parsed(data, message_length)

    @staticmethod
    def more_data_required(data):
        if data.options['max_message_len'] == MAX_MESSAGE_LENGTH_UNLIMITED:
            return MoreDataRequired(data), data
        buf_pos = data.buffer.pos
        already_read = buf_pos - data.start_pos
        accumulated = data.buffer.acc_length + data.buffer.queue_length
        if already_read + accumulated > data.options['max_message_len']:
            raise TransportParserError(f'Message too long')
        return MoreDataRequired(data), data

    @staticmethod
    def message_parsed(data, length):
        if isinstance(data.options['max_message_len'], int) and length > data.options['max_message_len']:
            raise TransportParserError(f'Message too long')
        return data.message, data

    @staticmethod
    def add_header(header, data):
        if isinstance(header, list):
            header_string = header[0]
            if len(header) > 1:
                rest = ''.join(header[1:])
            else:
                rest = ''
        else:
            header_string = header
            rest = ''
        try:
            header_name, header_value = header_string.split(':', 1)
            data.message.add(header_name.strip(), header_value + rest)
        except ValueError:
            raise TransportParserError(f'Cannot add header {header}: bad header')
