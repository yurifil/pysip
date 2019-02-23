from pysip import PySIPException
from pysip.uri.uri import Uri
from pysip.message.hnames import FROM_HEADER, TO_HEADER, CALLID_HEADER, CSEQ_HEADER, MAXFORWARDS_HEADER, VIA_HEADER, \
    KNOWN_HEADER_KEY_MAP, ALL_KNOWN_HEADERS_KEYS, PRINT_FORM_MAP
from pysip.message.method import Method
from pysip.message.hdr import Header, BaseSipHeader
from pysip.message.hnames import make_key
from pysip.message.message import TYPE_REQUEST, TYPE_RESPONSE, RequestType, ResponseType, Message
from pysip.message.hdr_fromto import FromToHeader
from pysip.message.hdr_cseq import CSeqHeader
from pysip.message.hdr_callid import CallIDHeader
from pysip.message.hdr_maxforwards import MaxForwardsHeader
from pysip.message.hdr_via import ViaHeader
from pysip.message.hdr_content_type import ContentTypeHeader
from pysip.message.hdr_route import RouteHeader
from pysip.message.hdr_allow import AllowHeader
from pysip.message.hdr_opttag_list import OptTagListHeader
from pysip.message.hdr_contact_list import ContactHeaderList
from pysip.message.hdr_expires import ExpiresHeader
from pysip.message.hdr_date import DateHeader
from pysip.reply import ReplyOptions, AUTO
from pysip.message.pysip_id import generate_token

from pysip.transport.parser import Parser, MoreDataRequired


REQUIRED_REQUESTS = 'requests'
REQUIRED_WITH_BODY = 'with_body'
REQUIRED_ALL = 'all'
REQUIRED_OPTIONAL = 'optional'


ALL = 'ALL'
# TODO: add type annotations here and there
# TODO: SipHeader interface is ugly, refactor needed


class SIPMessageParseError(PySIPException):
    pass


class SipMessageError(PySIPException):
    pass


class NotFound(object):
    pass


class SipMessage(object):
    def __init__(self, message=None):
        """Provides parse and validation routines and simple reply interface for SIP messages.
        Fields:
            raw_message (Message) - Message instance containing raw message
            method (Method) - SIP method parsed from request line for requests or from CSeq header for responses
            headers (dict) - dict of headers: keys are HdrKey objects, values are parsed headers
            user (str) - user data
        """
        if message is None:
            message = Message()
        self.raw_message = message
        self.method = None
        self._ruri = None
        self.headers = dict()
        self.user = None

    @property
    def user_data(self):
        """
        Raises:
            SipMessageError if no user data defined.
        """
        if self.user is None:
            raise SipMessageError(f'No user data')
        return self.user

    @user_data.setter
    def user_data(self, data):
        self.user = data

    def serialize(self):
        """
        Returns:
            String representation of SIP message.
        """
        return self.raw_message.serialize()

    def clear_user_data(self):
        """Resets SIP message user data"""
        self.user = None

    @property
    def type(self):
        return self.raw_message.type

    @type.setter
    def type(self, type_str):
        """
        Args:
            type_str (str)

        Raises:
            SipMessageError if type_str != request and type_str != response
        """
        if type_str == TYPE_RESPONSE:
            self.raw_message.type = ResponseType()
        elif type_str == TYPE_REQUEST:
            self.raw_message.type = RequestType()
        else:
            raise SipMessageError(f'Cannot set type {type_str}: invalid type')

    @property
    def ruri(self):
        return self._ruri

    @ruri.setter
    def ruri(self, uri):
        self._ruri = uri
        if uri is not None:
            self.raw_message.ruri = uri.uri

    @property
    def status(self):
        if self.type == TYPE_RESPONSE:
            return self.raw_message.status
        else:
            return None

    @status.setter
    def status(self, status):
        """
        Args:
            status (int)

        Raises:
            SipMessageError if SIP message type is request
        """
        if self.type == TYPE_REQUEST:
            raise SipMessageError(f'Cannot set status for request {self}')
        elif self.type == TYPE_RESPONSE:
            self.raw_message.status = status

    @property
    def from_header(self):
        return self.get(FROM_HEADER)

    @property
    def to(self):
        return self.get(TO_HEADER)

    @property
    def callid(self):
        return self.get(CALLID_HEADER)

    @property
    def cseq(self):
        return self.get(CSEQ_HEADER)

    @property
    def maxforwards(self):
        return self.get(MAXFORWARDS_HEADER)

    @property
    def topmost_via(self):
        return self.get(VIA_HEADER)

    @property
    def reason(self):
        """
        Returns:
            None if SIP message is request
            :str: reason from raw message (Message).
        """
        if self.type == TYPE_RESPONSE:
            return self.raw_message.reason
        elif self.type == TYPE_REQUEST:
            return None

    def find(self, header):
        """Find specified header

        Args:
            header (str or HeaderKey)

        Returns:
            BaseSipHeader subclass instance
        """
        if make_key(header) in self.headers:
            return self.headers[make_key(header)]
        else:
            try:
                hdr = SipHeader.parse_header(header, self)
            except SipHeaderError as e:
                raise e
            if not hdr or isinstance(hdr, NoHeader):
                return NotFound()
            return hdr

    def get(self, header):
        """Gets header specified by header name.

        Args:
            header (str or HeaderKey)

        Returns:
            BaseSipHeader subclass instance
        """
        try:
            hdr = self.find(header)
        except Exception as e:
            raise e
        if isinstance(hdr, NotFound):
            raise SipMessageError(f'Cannot get header {header}: no header')
        return hdr

    def set(self, header_name, header_value):
        """Sets SipMessage header to specified value.

        Args:
            header_name (str)
            header_value (BaseSipHeader subclass instance)
        """
        SipHeader.set_header(header_name, header_value, self)

    @staticmethod
    def copy(header_name, src_msg, dst_msg):
        """Copies specified header from source SIP message to destination SIP message

        Args:
            header_name (str)
            src_msg (SipMessage)
            dst_msg (SipMessage)
        """
        SipHeader.copy_header(header_name, src_msg, dst_msg)

    def remove(self, header_name):
        """Removes specified header from SIP message.

        Args:
            header_name (str or HeaderKey): header name to be removed.
        """
        SipHeader.remove_header(header_name, self)

    def has_body(self):
        """Returns:
            True if SIP message body is not empty,
            False otherwise.
        """
        return self.raw_message.body

    def remove_body(self):
        """Resets SIP message body."""
        self.raw_message.body = ''

    def raw_header(self, header_name):
        """Get raw header.

        Args:
            header_name (str)

        Returns:
            Header object from SipMessage.raw_message
        """
        return self.raw_message.get(header_name)

    def set_raw_header(self, raw_header):
        """Sets raw header to SIP message.

        Args:
            raw_header (Header)
        """
        SipHeader.set_raw_header(raw_header, self)

    @staticmethod
    def parse(message, headers_list):
        """Parses specified headers from string, SipMessage or Message objects and sets it to SIP message.

        Args:
            message (str or Message or SipMessage)
            headers_list (list or ALL): headers to be parsed

        Returns:
            :obj:SipMessage
        """
        if isinstance(message, SipMessage):
            known_headers = headers_list
            if headers_list == ALL:
                known_headers = KNOWN_HEADER_KEY_MAP.values()
            already_parsed = message.headers.keys()
            headers_to_parse = set(known_headers).difference(set(already_parsed))
            for header in headers_to_parse:
                SipMessage.maybe_parse_header(header, message)
            return message
        elif isinstance(message, str):
            p = Parser.new_dgram(message)
            try:
                parsed_msg, data = Parser.parse(p)
            except Exception as e:
                raise SipMessageError(f'Cannot parse message {message}: {e}')
            if isinstance(parsed_msg, MoreDataRequired):
                raise SipMessageError(f'Cannot parse message {message}: truncated message.')
            return SipMessage.parse(parsed_msg, headers_list)
        elif isinstance(message, Message):
            headers_to_parse = headers_list
            if headers_list == ALL:
                headers_to_parse = KNOWN_HEADER_KEY_MAP.keys()
            maybe_msg = SipMessage.create_from_raw(message)
            for header in headers_to_parse:
                header_key = KNOWN_HEADER_KEY_MAP[header.lower()]
                SipMessage.maybe_parse_header(header_key, maybe_msg)
            SipMessage.parse_validate_cseq(maybe_msg)
            return maybe_msg
        else:
            raise SipMessageError(f'Cannot parse message {message}: wrong type {type(message)}')

    @staticmethod
    def parse_validate_cseq(message):
        """
        Args:
            message (SipMessage)

        Returns:
            True if no cseq is defined,
            True if SIP message method equals to method defined in CSeq header,
            False otherwise.
        """
        cseq = message.headers.get(make_key('cseq'))
        if cseq:
            return message.method == cseq.method
        return True

    @staticmethod
    def maybe_parse_header(header, message):
        """Sets SipMessage header if it can be parsed and does nothing otherwise.

        Args:
            header (str or HeaderKey): header name.
            message (SipMessage): SIP message to be changed.
        """
        hdr = SipHeader.parse_header(header, message)
        if isinstance(hdr, NoHeader):
            # do nothing, message is not changed
            pass
        else:
            message.headers[header] = hdr

    @staticmethod
    def create_from_raw(raw_message):
        """Initializes SipMessage object using parameters of raw message: sets raw_message, method and ruri fields of
        SipMessage object.

        Args:
            raw_message (Message)

        Returns:
            :obj:SipMessage
        """
        sip_msg = SipMessage()
        sip_msg.raw_message = raw_message
        sip_msg.method = SipMessage.method_from_raw(raw_message)
        sip_msg.ruri = SipMessage.ruri_from_raw(raw_message)
        return sip_msg

    @staticmethod
    def method_from_raw(raw_message):
        """Returns method of raw message if it has type request and method defined in CSeq header if raw message has
        type response.

        Args:
            raw_message (Message)

        Returns:
            :obj:Method
        """
        if raw_message.type == TYPE_REQUEST:
            return raw_message.method
        elif raw_message.type == TYPE_RESPONSE:
            cseq_hdr = raw_message.get(CSEQ_HEADER)
            cseq = CSeqHeader.parse(cseq_hdr)
            return cseq.method

    @staticmethod
    def ruri_from_raw(raw_message):
        """Returns None if raw message has type response and Uri object if raw message has type request.

        Args:
            raw_message (Message)

        Returns:
            :obj:Uri or None
        """
        if raw_message.type == TYPE_RESPONSE:
            return None
        elif raw_message.type == TYPE_REQUEST:
            return Uri(raw_message.ruri)

    def add(self, headername, value):
        """Adds value to header.

        Args:
            headername (str or HeaderKey): header name.
            value (str): header value.
        """
        if not make_key(headername) in self.headers:
            self.headers[make_key(headername)] = Header(headername)
        if not isinstance(value, str):
            raise SipMessageError(f'Cannot add header {headername}={value} to message {self.serialize()}: value should be '
                                  f'string not ({type(value)}).')
        self.headers[make_key(headername)].add_value(value.lstrip())

    def reply(self, reply_opts):
        """Returns SipMessage response with parameters defined by self and reply_opts.

        Args:
            reply_opts (ReplyOptions): parameters of SIP response.

        Returns:
            :obj:SipMessage
        """
        if isinstance(reply_opts, int):
            reply_opts = ReplyOptions(reply_opts)
        return self._reply_implementation(reply_opts)

    @staticmethod
    def new_reply(status, method, reason=None):
        """Initializes empty response SIP message

        Args:
            status (int)
            method (Method)
            reason (str)

        Returns:
            :obj:SipMessage response with parametrized status, method and reason.
        """
        msg = Message()
        msg.type = ResponseType(status=status, reason=reason)
        sip_msg = SipMessage()
        sip_msg.raw_message = msg
        sip_msg.method = method
        sip_msg.ruri = None
        return sip_msg

    # When a 100 (Trying) response is generated, any
    # Timestamp header field present in the request MUST be
    # copied into this 100 (Trying) response.
    #
    # TODO: If there is a delay in generating the response, the UAS
    # SHOULD add a delay value into the Timestamp value in the response.
    # This value MUST contain the difference between the time of sending
    # of the response and receipt of the request, measured in seconds.
    def maybe_copy_timestamp(self, status, reply_message):
        if status == 100:
            SipHeader.copy_header('timestamp', self, reply_message)

    # 8.2.6.2 Headers and Tags
    #
    # However, if the To header field in the request did not contain a
    # tag, the URI in the To header field in the response MUST equal the
    # URI in the To header field; additionally, the UAS MUST add a tag to
    # the To header field in the response (with the exception of the 100
    # (Trying) response, in which a tag MAY be present).
    def maybe_set_to_tag(self, reply_options, reply_message):
        """Sets tag field of To header in reply_message to:
        - to_tag defined in reply options
        - random token if to tag is not defined in reply options

        Args:
            reply_options (ReplyOptions): reply options object
            reply_message (SipMessage): sip message that will have to.tag field set
        """
        if reply_options.status == 100:
            SipHeader.copy_header(TO_HEADER, self, reply_message)
        else:
            if reply_options.to_tag == AUTO:
                tag = generate_token(8)
            else:
                tag = reply_options.to_tag
            to = self.get(TO_HEADER)
            to.tag = tag
            reply_message.set(TO_HEADER, to)

    # 8.2.6 Generating the Response
    #
    # Note valid parameters:
    # 1. SipMsg has to_tag
    # 2. Reply contains to_tag
    # 3. Reply is 100 Trying
    #
    # Otherwise function generates error.
    def _reply_implementation(self, reply):
        """Inner implementation of reply method.

        Args:
            reply (ReplyOptions):

        Returns:
            :obj:SipMessage
        """
        status = reply.status
        method = self.method
        reply_msg = SipMessage.new_reply(status, method=method)
        # 8.2.6.1 Sending a Provisional Response
        # When a 100 (Trying) response is generated, any
        # Timestamp header field present in the request MUST be
        # copied into this 100 (Trying) response.
        self.maybe_copy_timestamp(status, reply_msg)
        # 8.2.6.2 Headers and Tags
        #
        # The From field of the response MUST equal the From header field
        # of the request.  The Call-ID header field of the response MUST
        # equal the Call-ID header field of the request.  The CSeq header
        # field of the response MUST equal the CSeq field of the request.
        # The Via header field values in the response MUST equal the Via
        # header field values in the request and MUST maintain the same
        # ordering
        for header_name in [FROM_HEADER, CALLID_HEADER, CSEQ_HEADER, VIA_HEADER]:
            SipHeader.copy_header(header_name, self, reply_msg)
        to_header = self.get(TO_HEADER)
        # If a request contained a To tag in the request, the
        # To header field in the response MUST equal that of
        # the request.
        if to_header.tag is not None:
            SipHeader.copy_header(TO_HEADER, self, reply_msg)
        else:
            self.maybe_set_to_tag(reply, reply_msg)
        return reply_msg


class NoHeader(object):
    pass


class DescriptionError(PySIPException):
    pass


class Description(object):
    def __init__(self, required=None, header_class=None):
        if not issubclass(header_class, BaseSipHeader):
            raise Description(f'Cannot initialize Description({required}, {header_class}): header_class should be a '
                              f'subclass of BaseSipHeader')
        self.required = required
        self.header_class = header_class

    def __repr__(self):
        return f'{self.__class__.__name__}: required {self.required}, header_class: {self.header_class.__name__}'

    def parse_header(self, header):
        return self.header_class.parse(header)


class SipHeaderError(PySIPException):
    pass


class RequiredEssentials(object):
    def __init__(self, sip_message):
        self.type = sip_message.type
        self.method = sip_message.method
        self.status = sip_message.status
        self.has_body = sip_message.has_body


class SipHeader(object):
    @staticmethod
    def copy_header(header, src_msg, dst_msg):
        """

        Args:
            header: header name to be copied
            src_msg (SipMessage): SipMessage to be copied from
            dst_msg (SipMessage): message to be copied to

        Returns:

        """
        if make_key(header) in src_msg.headers:
            dst_msg.headers[make_key(header)] = src_msg.headers[make_key(header)]
            SipHeader.copy_raw_header(make_key(header), src_msg, dst_msg)
        if make_key(header) not in ALL_KNOWN_HEADERS_KEYS:
            SipHeader.copy_raw_header(make_key(header), src_msg, dst_msg)

    @staticmethod
    def copy_raw_header(header_key, src_msg, dst_msg):
        dst_msg.raw_message.set_header(src_msg.raw_message.get(header_key))

    @staticmethod
    def copy_headers(header_list, src_msg, dst_msg):
        pass

    @staticmethod
    def set_header(header_name, header_value, msg):
        descr = SipHeader.header_descr(header_name)
        print_name = PRINT_FORM_MAP.get(make_key(header_name), header_name)
        raw_hdr = Header(print_name)
        raw_hdr.add_value(header_value.assemble())
        if not raw_hdr.values:
            msg.headers.pop(make_key(header_name), None)
            msg.raw_message.delete_header(make_key(header_name))
        else:
            msg.headers[make_key(header_name)] = header_value
            msg.raw_message.set_header(raw_hdr)

    @staticmethod
    def set_raw_header(header, msg):
        msg.raw_message.set_header(header)
        if header.key in ALL_KNOWN_HEADERS_KEYS:
            try:
                msg.headers.pop(header.key)
                msg = SipMessage.parse(msg, [header.name])
            except KeyError:
                pass

    @staticmethod
    def remove_header(header, msg):
        msg.headers.pop(make_key(header), None)
        msg.raw_message.delete_header(header)

    @staticmethod
    def parse_header_by_descr(descr, header):
        pass

    @staticmethod
    def parse_header(header, msg):
        descr = SipHeader.header_descr(header)
        hdr = SipHeader.get_header(header, descr, msg)
        if isinstance(hdr, NoHeader):
            return hdr
        if isinstance(hdr, Header):
            return descr.parse_header(hdr)
        raise SipHeaderError(f'Cannot parse header {header}: header should be of type Header not {type(header)}')

    @staticmethod
    def get_header(header, descr, msg):
        hdr = msg.raw_message.get(header)
        if not hdr.values:
            if SipHeader.is_required(msg, descr.required):
                raise SipHeaderError(f'Cannot get required header {header}: no header.')
            return NoHeader()
        return hdr

    @staticmethod
    def is_required(msg, required):
        """

        Args:
            msg (:obj:SipMessage): instance of pysip.message.sipmsg.SipMessage class.
            required (bool): required flag taken from header's description

        Returns:
            True if header is required,
            False otherwise.
        """
        if required == REQUIRED_ALL:
            return True
        elif required == REQUIRED_OPTIONAL:
            return False
        elif isinstance(msg, RequiredEssentials):
            if msg.type == TYPE_REQUEST and required == REQUIRED_REQUESTS:
                return True
            elif msg.has_body() and required == REQUIRED_WITH_BODY:
                return True
            else:
                return False
        elif isinstance(msg, SipMessage):
            return SipHeader.is_required(RequiredEssentials(msg), required)
        else:
            raise SipHeaderError(f'Cannot decide if {msg} is required: msg should be of type SipMessage or '
                                 f'RequiredEssentials, not {type(msg)}')

    @staticmethod
    def header_descr(header):
        header_key = make_key(header)
        if header_key == make_key('from') or header_key == make_key('to'):
            return Description(required=REQUIRED_ALL, header_class=FromToHeader)
        elif header_key == make_key('cseq'):
            return Description(required=REQUIRED_ALL, header_class=CSeqHeader)
        elif header_key == make_key('callid') or header_key == make_key('call-id'):
            return Description(required=REQUIRED_ALL, header_class=CallIDHeader)
        elif header_key == make_key('max-forwards'):
            return Description(required=REQUIRED_OPTIONAL, header_class=MaxForwardsHeader)
        elif header_key == make_key('topmost_via') or header_key == make_key('via'):
            return Description(required=REQUIRED_REQUESTS, header_class=ViaHeader)
        elif header_key == make_key('content-type'):
            return Description(required=REQUIRED_WITH_BODY, header_class=ContentTypeHeader)
        elif header_key == make_key('route') or header_key == make_key('record-route'):
            return Description(required=REQUIRED_OPTIONAL, header_class=RouteHeader)
        elif header_key == make_key('allow'):
            return Description(required=REQUIRED_OPTIONAL, header_class=AllowHeader)
        elif header_key in (make_key('supported'), make_key('unsupported'), make_key('require'),
                            make_key('proxy-require')):
            return Description(required=REQUIRED_OPTIONAL, header_class=OptTagListHeader)
        elif header_key == make_key('contact'):
            return Description(required=REQUIRED_OPTIONAL, header_class=ContactHeaderList)
        elif header_key == make_key('expires') or header_key == make_key('min-expires'):
            return Description(required=REQUIRED_OPTIONAL, header_class=ExpiresHeader)
        elif header_key == make_key('date'):
            return Description(required=REQUIRED_OPTIONAL, header_class=DateHeader)


