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

'''
-spec reply(ersip_reply:options() | ersip_status:code(), sipmsg()) -> sipmsg().
reply(Code, #sipmsg{} = SipMsg) when is_integer(Code) andalso Code >= 100 andalso Code =< 699 ->
    reply_impl(ersip_reply:new(Code), SipMsg);
reply(Reply, #sipmsg{} = SipMsg) ->
    reply_impl(Reply, SipMsg).
    
-spec new_reply(Status, Reason, Method) -> sipmsg() when
      Status :: ersip_status:code(),
      Reason :: ersip_status:reason() | undefined,
      Method :: ersip_method:method().
new_reply(Status, Reason, Method) ->
    RawMsg = ersip_msg:new(),
    RawMsg1 = ersip_msg:set([{type,   response},
                             {status, Status},
                             {reason, Reason}],
                            RawMsg),
    #sipmsg{raw    = RawMsg1,
            method = Method,
            ruri   = undefined
           }.

%% 8.2.6 Generating the Response
%%
%% Note valid parameters:
%% 1. SipMsg has to_tag
%% 2. Reply contains to_tag
%% 3. Reply is 100 Trying
%%
%% Otherwise function generates error.
-spec reply_impl(ersip_reply:options(), sipmsg()) -> sipmsg().
reply_impl(Reply, SipMsg) ->
    Status = ersip_reply:status(Reply),
    Method = method(SipMsg),
    RSipMsg0 = new_reply(Status, ersip_reply:reason(Reply), Method),
    %% 8.2.6.1 Sending a Provisional Response
    %% When a 100 (Trying) response is generated, any
    %% Timestamp header field present in the request MUST be
    %% copied into this 100 (Trying) response.
    RSipMsg1 = maybe_copy_timestamp(Status, SipMsg, RSipMsg0),

    %% 8.2.6.2 Headers and Tags
    %%
    %% The From field of the response MUST equal the From header field
    %% of the request.  The Call-ID header field of the response MUST
    %% equal the Call-ID header field of the request.  The CSeq header
    %% field of the response MUST equal the CSeq field of the request.
    %% The Via header field values in the response MUST equal the Via
    %% header field values in the request and MUST maintain the same
    %% ordering
    RSipMsg2 = ersip_siphdr:copy_headers(
                 [from, callid, cseq, <<"via">>],
                 SipMsg, RSipMsg1),

    RSipMsg3 =
        case ersip_hdr_fromto:tag(get(to, SipMsg)) of
            {tag, _} ->
                %% If a request contained a To tag in the request, the
                %% To header field in the response MUST equal that of
                %% the request.
                ersip_siphdr:copy_header(to, SipMsg, RSipMsg2);
            undefined ->
                maybe_set_to_tag(Reply, SipMsg, RSipMsg2)
        end,
    RSipMsg3.

%% When a 100 (Trying) response is generated, any
%% Timestamp header field present in the request MUST be
%% copied into this 100 (Trying) response.
%%
%% TODO: If there is a delay in generating the response, the UAS
%% SHOULD add a delay value into the Timestamp value in the response.
%% This value MUST contain the difference between the time of sending
%% of the response and receipt of the request, measured in seconds.
-spec maybe_copy_timestamp(ersip_status:code(), sipmsg(), sipmsg()) -> sipmsg().
maybe_copy_timestamp(100, SipMsg, RSipMsg) ->
    ersip_siphdr:copy_header(<<"timestamp">>, SipMsg, RSipMsg);
maybe_copy_timestamp(_, _, RSipMsg) ->
    RSipMsg.

'''