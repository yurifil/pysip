from pysip.message.hnames import FROM_HEADER, TO_HEADER, CALLID_HEADER, CSEQ_HEADER, MAXFORWARDS_HEADER, VIA_HEADER, \
    ALL_KNOWN_HEADERS
from pysip import PySIPException
from pysip.message.message import TYPE_REQUEST, TYPE_RESPONSE
from pysip.message.status import reason_phrase
from pysip.message.hdr import Header
from pysip.message.sip_hdr import parse_header, set_header, copy_header, remove_header, set_raw_header, \
    SipHeaderError, NoHeader
from pysip.uri.uri import Uri


ALL = 'all'
# TODO: add type annotations here and there


class SIPMessageParseError(PySIPException):
    pass


class SipMessageError(PySIPException):
    pass


class NotFound(object):
    pass


class SipMessage(object):
    def __init__(self, message=None):
        self.raw_message = None
        self.method = None
        self._ruri = None
        self.headers = dict()

    @property
    def type(self):
        return self.raw_message.type

    @property
    def ruri(self):
        return self._ruri

    @ruri.setter
    def ruri(self, uri):
        self._ruri = uri
        self.raw_message.ruri = uri.assemble()

    @property
    def status(self):
        if self.type == TYPE_RESPONSE:
            return self.raw_message.status
        else:
            return None

    @status.setter
    def status(self, status):
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
        return self.get('topmost_via')

    @property
    def reason(self):
        if self.type == TYPE_RESPONSE:
            return self.raw_message.reason
        elif self.type == TYPE_REQUEST:
            return None

    def find(self, header):
        if header in self.headers:
            return self.headers[header]
        else:
            try:
                hdr = parse_header(header, self)
            except SipHeaderError as e:
                raise e
            if not hdr:
                return NotFound()
            return hdr

    def get(self, header):
        try:
            hdr = self.find(header)
        except Exception as e:
            raise e
        if isinstance(hdr, NotFound):
            raise SipMessageError(f'Cannot get header {header}: no header')
        return hdr

    def set(self, header_name, header_value):
        set_header(header_name, header_value, self)

    @staticmethod
    def copy(header_name, src_msg, dst_msg):
        copy_header(header_name, src_msg, dst_msg)

    def remove(self, header_name):
        remove_header(header_name, self)

    def has_body(self):
        return not self.raw_message.is_empty()

    def remove_body(self):
        self.raw_message.body = ''

    def raw_header(self, header_name):
        self.raw_message.get(header_name)

    def set_raw_header(self, raw_header):
        set_raw_header(raw_header, self)

    @staticmethod
    def parse(message, headers_list):
        if isinstance(message, SipMessage):
            known_headers = headers_list
            if headers_list == ALL:
                known_headers = ALL_KNOWN_HEADERS
            already_parsed = message.headers.keys()
            headers_to_parse = set(known_headers).difference(set(already_parsed))
            for header in headers_to_parse:
                maybe_parse_header()
        elif isinstance(message, str):
            p = pysip_parser.new_dgram(message)
            try:
                parsed_msg = pysip.parser.parse(p)
            except Exception as e:
                raise SipMessageError(f'Cannot parse message {message}: {e}')
            if parsed_msg.more_data():
                raise SipMessageError(f'Cannot parse message {message}: truncated message.')
            return SipMessage.parse(parsed_msg, headers_list)
        else:
            known_headers = headers_list
            if headers_list == ALL:
                known_headers = ALL_KNOWN_HEADERS
            maybe_msg = SipMessage.create_from_raw(message)
            for header in headers_list:
                maybe_parse_header()


'''
%% @doc Parse Raw message and transform it to SIP message or parse
%% additional headers of SIP message.
-spec parse(ersip_msg:message() | sipmsg() | binary() | iolist(), [known_header()] | all) -> Result when
      Result :: {ok, sipmsg()}
              | {error, term()}.
parse(#sipmsg{} = SipMsg, all) ->
    AlreadyParsed = maps:keys(headers(SipMsg)),
    HeadersToParse = ersip_siphdr:all_known_headers() -- AlreadyParsed,
    MaybeMsg = {ok, SipMsg},
    lists:foldl(fun maybe_parse_header/2, MaybeMsg, HeadersToParse);
parse(#sipmsg{} = SipMsg, Headers) ->
    AlreadyParsed = maps:keys(headers(SipMsg)),
    HeadersToParse = Headers -- AlreadyParsed,
    MaybeMsg = {ok, SipMsg},
    lists:foldl(fun maybe_parse_header/2, MaybeMsg, HeadersToParse);
parse(SipMsgBin, What) when is_binary(SipMsgBin) ->
    P  = ersip_parser:new_dgram(SipMsgBin),
    case ersip_parser:parse(P) of
        {{ok, PMsg}, _P2} ->
            parse(PMsg, What);
        {{error, Reason}, _} ->
            {error, {generic_parse_error, Reason}};
        {more_data, _} ->
            {error, truncated_message}
    end;
parse(RawMsg, all) ->
    parse(RawMsg, ersip_siphdr:all_known_headers());
parse(RawMsg, Headers) ->
    MaybeMsg0 = create_from_raw(RawMsg),
    MaybeMsg1 = lists:foldl(fun maybe_parse_header/2, MaybeMsg0, Headers),
    lists:foldl(fun({ValFun, ErrorType}, {ok, SipMsg} = MaybeMsg) ->
                        case ValFun(SipMsg) of
                            true ->
                                MaybeMsg;
                            false ->
                                {error, {ErrorType, SipMsg}}
                        end;
                   (_, {error, _} = Error) ->
                        Error
                end,
                MaybeMsg1,
                [{fun parse_validate_cseq/1, invalid_cseq}]).


-spec raw_message(sipmsg()) -> ersip_msg:message().
raw_message(#sipmsg{raw = R}) ->
    R.

-spec raw_header(HdrName :: binary(), sipmsg()) -> ersip_hdr:header().
raw_header(HdrName, #sipmsg{} = Msg) when is_binary(HdrName) ->
    ersip_msg:get(HdrName, raw_message(Msg)).

-spec set_raw_header(ersip_hdr:header(), sipmsg()) -> {ok, ersip_sipmsg:sipmsg()} | {error, term()}.
set_raw_header(RawHdr, #sipmsg{} = SipMsg) ->
    ersip_siphdr:set_raw_header(RawHdr, SipMsg).

-spec remove_body(sipmsg()) -> sipmsg().
remove_body(#sipmsg{} = SipMsg) ->
    RawMsg = raw_message(SipMsg),
    RawMsgNoBody = ersip_msg:set(body, [], RawMsg),
    set_raw_message(RawMsgNoBody, SipMsg).
    

-spec has_body(sipmsg()) -> boolean().
has_body(#sipmsg{} = Msg) ->
    not ersip_iolist:is_empty(ersip_msg:get(body, raw_message(Msg))).

-spec remove(ersip_hnames:name_forms(), sipmsg()) -> sipmsg().
remove(HdrName, SipMsg) ->
    ersip_siphdr:remove_header(HdrName, SipMsg).

-spec copy(ersip_hnames:name_forms(), Src :: sipmsg(), Dst :: sipmsg()) -> sipmsg().
copy(HdrNameForm, #sipmsg{} = SrcMsg, #sipmsg{} = DstMsg) ->
    ersip_siphdr:copy_header(HdrNameForm, SrcMsg, DstMsg).

set(HdrAtom, Value, #sipmsg{} = Msg) ->
    ersip_siphdr:set_header(HdrAtom, Value, Msg).

-spec find(known_header(), sipmsg()) -> Result when
      Result :: {ok, term()}
              | not_found
              | {error, term()}.
find(HdrAtom, #sipmsg{headers = H} = Msg) ->
    case maps:find(HdrAtom, H) of
        {ok, Value} ->
            {ok, Value};
        error ->
            case ersip_siphdr:parse_header(HdrAtom, Msg) of
                {ok, no_header} ->
                    not_found;
                {ok, Value} ->
                    {ok, Value};
                {error, {no_required_header, _}} ->
                    not_found;
                {error, _ } = Error ->
                    Error
            end
    end.

-spec get(known_header(), sipmsg()) -> Value when
      Value :: term().
get(HdrAtom, #sipmsg{} = Msg) ->
    case find(HdrAtom, Msg) of
        {ok, Value} ->
            Value;
        not_found ->
            error({error, {no_header, HdrAtom}});
        {error, _} = Error ->
            error(Error)
    end.

-spec callid(sipmsg()) -> ersip_hdr_callid:callid().
callid(#sipmsg{} = SipMsg) ->
    get(callid, SipMsg).

-spec cseq(sipmsg()) -> ersip_hdr_cseq:cseq().
cseq(#sipmsg{} = SipMsg) ->
    get(cseq, SipMsg).

-spec maxforwards(sipmsg()) -> ersip_hdr_maxforwards:maxforwards().
maxforwards(#sipmsg{} = SipMsg) ->
    get(maxforwards, SipMsg).

-spec topmost_via(sipmsg()) -> ersip_hdr_via:via().
topmost_via(#sipmsg{} = SipMsg) ->
    get(topmost_via, SipMsg).

-spec reason(ersip_sipmsg:sipmsg()) -> undefined | binary().
reason(#sipmsg{} = SipMsg) ->
    case type(SipMsg) of
        request ->
            undefined;
        response ->
            ersip_msg:get(reason, raw_message(SipMsg))
    end.
    
-spec from(sipmsg()) -> ersip_hdr_fromto:fromto().
from(#sipmsg{} = SipMsg) ->
    get(from, SipMsg).

-spec to(sipmsg()) -> ersip_hdr_fromto:fromto().
to(#sipmsg{} = SipMsg) ->
    get(to, SipMsg).
    
-spec type(ersip_sipmsg:sipmsg()) -> ersip_msg:type().
type(#sipmsg{} = Msg) ->
    ersip_msg:get(type, raw_message(Msg)).

-spec method(ersip_sipmsg:sipmsg()) -> ersip_method:method().
method(#sipmsg{method = Method}) ->
    Method.

-spec ruri(ersip_sipmsg:sipmsg()) -> ersip_uri:uri().
ruri(#sipmsg{ruri = RURI}) ->
    RURI.

-spec set_ruri(ersip_uri:uri(), sipmsg()) -> sipmsg().
set_ruri(URI, #sipmsg{} = SipMsg) ->
    SipMsg0 = SipMsg#sipmsg{ruri = URI},
    RawMsg = ersip_msg:set(ruri, ersip_uri:assemble(URI), raw_message(SipMsg0)),
    set_raw_message(RawMsg, SipMsg0).

-spec status(ersip_sipmsg:sipmsg()) -> undefined | ersip_status:code().
status(#sipmsg{} = SipMsg) ->
    case type(SipMsg) of
        request ->
            undefined;
        response ->
            ersip_msg:get(status, raw_message(SipMsg))
    end.

-spec set_status(ersip_status:code(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
set_status(Code, #sipmsg{} = SipMsg) ->
    case type(SipMsg) of
        request ->
            error({api_error, {<<"cannot set status of request">>, SipMsg}});
        response ->
            RawMsg = ersip_msg:set(status, Code, raw_message(SipMsg)),
            set_raw_message(RawMsg, SipMsg)
    end.
%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message
%%
%% Contains raw message (if any) and parsed headers.
%%

-module(ersip_sipmsg).

%% API exports
-export([%% First line manipulation:
         type/1,
         method/1,
         ruri/1,
         set_ruri/2,
         status/1,
         set_status/2,
         reason/1,

         %% Required headers. Function raise error if message does not
         %% contain corresponding header.
         from/1,
         to/1,
         callid/1,
         cseq/1,
         maxforwards/1,
         topmost_via/1,

         %% Headers manipulation:
         find/2,
         get/2,
         set/3,
         copy/3,
         remove/2,

         %% Body manipulation:
         has_body/1,
         remove_body/1,

         %% Underlying message manipulation:
         raw_message/1,
         raw_header/2,
         set_raw_header/2,

         %% Parse and build message
         parse/2,
         serialize/1,
         serialize_bin/1,

         %% SIP-specific
         new_request/2,
         reply/2,

         %% Metadata manipulation:
         source/1,
         user_data/1,
         set_user_data/2,
         clear_user_data/1
        ]).

%% Non-API exports.
-export([headers/1,
         set_headers/2,
         set_raw_message/2]).

-export_type([sipmsg/0,
              known_header/0
             ]).
%%%===================================================================
%%% Types
%%%===================================================================

-record(sipmsg, {raw = undefined :: ersip_msg:message(),
                 method          :: ersip_method:method(),
                 ruri            :: ersip_uri:uri() | undefined,
                 headers = #{}   :: headers(),
                 user            :: term() %% User data carried with message
                }).

-type sipmsg()       :: #sipmsg{}.
-type known_header() :: ersip_siphdr:known_header().
-type headers()      :: #{from         => ersip_hdr_fromto:fromto(),
                          to           => ersip_hdr_fromto:fromto(),
                          callid       => ersip_hdr_callid:callid(),
                          cseq         => ersip_hdr_cseq:cseq(),
                          maxforwards  => ersip_hdr_maxforwards:maxforwards(),
                          topmost_via  => ersip_hdr_via:via()
                         }.

%%%===================================================================
%%% API
%%%===================================================================






-spec set(known_header(), Value :: term(), sipmsg()) -> Value when
      Value :: term().








-spec serialize(sipmsg()) -> iolist().
serialize(#sipmsg{} = SipMsg) ->
    ersip_msg:serialize(raw_message(SipMsg)).

-spec serialize_bin(sipmsg()) -> binary().
serialize_bin(#sipmsg{} = SipMsg) ->
    iolist_to_binary(serialize(SipMsg)).

%% Creating new request. To be more generic headers (even required)
%% are not automatically generated.
-spec new_request(ersip_method:method(), ersip_uri:uri()) -> sipmsg().
new_request(Method, RURI) ->
    RawMsg = ersip_msg:new(),
    RawMsg1 = ersip_msg:set([{type,   request},
                             {method, Method},
                             {ruri,   ersip_uri:assemble(RURI)}],
                            RawMsg),
    #sipmsg{raw    = RawMsg1,
            method = Method,
            ruri   = RURI
           }.

-spec reply(ersip_reply:options() | ersip_status:code(), sipmsg()) -> sipmsg().
reply(Code, #sipmsg{} = SipMsg) when is_integer(Code) andalso Code >= 100 andalso Code =< 699 ->
    reply_impl(ersip_reply:new(Code), SipMsg);
reply(Reply, #sipmsg{} = SipMsg) ->
    reply_impl(Reply, SipMsg).

-spec source(sipmsg()) -> undefined | ersip_source:source().
source(#sipmsg{} = SipMsg) ->
    ersip_msg:source(raw_message(SipMsg)).

-spec user_data(sipmsg()) -> term().
user_data(#sipmsg{user = undefined}) ->
    error({error, no_user_data});
user_data(#sipmsg{user = {set, User}}) ->
    User.

-spec set_user_data(term(), sipmsg()) -> sipmsg().
set_user_data(Data, #sipmsg{} = SipMsg) ->
    SipMsg#sipmsg{user = {set, Data}}.

-spec clear_user_data(sipmsg()) -> sipmsg().
clear_user_data(#sipmsg{} = SipMsg) ->
    SipMsg#sipmsg{user = undefined}.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec set_raw_message(ersip_msg:message(), sipmsg()) -> sipmsg().
set_raw_message(RawMsg, #sipmsg{} = SipMsg) ->
    SipMsg#sipmsg{raw = RawMsg}.

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

%%%
%%% Getters/Setters
%%%
-spec headers(sipmsg()) -> headers().
headers(#sipmsg{headers = H}) ->
    H.

-spec set_headers(headers(), sipmsg()) -> sipmsg().
set_headers(H, #sipmsg{} = M) ->
    M#sipmsg{headers = H}.

%%%
%%% Parsing infrastructure
%%%
-spec create_from_raw(ersip_msg:message()) -> maybe_sipmsg().
create_from_raw(RawMsg) ->
    MaybeMethod = method_from_raw(RawMsg),
    MaybeRURI   = ruri_from_raw(RawMsg),
    case fold_maybes([MaybeMethod, MaybeRURI]) of
        [Method, RURI] ->
            {ok,
             #sipmsg{method = Method,
                     ruri   = RURI,
                     raw    = RawMsg
                    }
            };
        {error, _} = Error ->
            Error
    end.

-type maybe_sipmsg() :: {ok, sipmsg()}
                      | {error, term()}.

-spec maybe_parse_header(known_header(), maybe_sipmsg()) -> maybe_sipmsg().
maybe_parse_header(_, {error, _} = Err) ->
    Err;
maybe_parse_header(Hdr, {ok, Msg}) ->
    parse_header(Hdr, Msg).

-spec parse_header(known_header(), sipmsg()) ->  maybe_sipmsg().
parse_header(HdrAtom, Msg) when is_atom(HdrAtom) ->
    case ersip_siphdr:parse_header(HdrAtom, Msg) of
        {ok, no_header} ->
            {ok, Msg};
        {ok, Value} ->
            Headers = headers(Msg),
            NewHeaders = Headers#{HdrAtom => Value},
            {ok, set_headers(NewHeaders, Msg)};
        {error, Reason} ->
            {error, {header_error, {HdrAtom, Reason}}}
    end.

-spec method_from_raw(ersip_msg:message()) -> MaybeMethod when
      MaybeMethod :: {ok, ersip_method:method()}
                   | {error, {invalid_cseq, term()}}.
method_from_raw(RawMsg) ->
    case ersip_msg:get(type, RawMsg) of
        request ->
            {ok, ersip_msg:get(method, RawMsg)};
        response ->
            CSeqHdr = ersip_msg:get(<<"cseq">>, RawMsg),
            case ersip_hdr_cseq:parse(CSeqHdr) of
                {ok, CSeq} ->
                    {ok, ersip_hdr_cseq:method(CSeq)};
                {error, Reason} ->
                    {error, {invalid_cseq, Reason}}
            end
    end.

-spec ruri_from_raw(ersip_msg:message()) -> MaybeRURI when
      MaybeRURI :: {ok, ersip_uri:uri() | undefined}
                 | {error, {invalid_ruri, term()}}.
ruri_from_raw(Msg) ->
    case ersip_msg:get(type, Msg) of
        request ->
            URIBin = iolist_to_binary(ersip_msg:get(ruri, Msg)),
            case ersip_uri:parse(URIBin) of
                {ok, _} = R ->
                    R;
                {error, Reason} ->
                    {error, {invalid_ruri, Reason}}
            end;
        response ->
            {ok, undefined}
    end.

fold_maybes(MaybesList) ->
    lists:foldr(fun(_, {error, _} = Error) ->
                        Error;
                   ({ok, Result}, Acc) ->
                        [Result | Acc];
                   ({error, _} = Error, _) ->
                        Error
                end,
                [],
                MaybesList).

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

%% 8.2.6.2 Headers and Tags
%%
%% However, if the To header field in the request did not contain a
%% tag, the URI in the To header field in the response MUST equal the
%% URI in the To header field; additionally, the UAS MUST add a tag to
%% the To header field in the response (with the exception of the 100
%% (Trying) response, in which a tag MAY be present).
-spec maybe_set_to_tag(ersip_reply:options(), SrcSipMsg, DstSipMsg) -> ResultSipMsg when
      SrcSipMsg :: sipmsg(),
      DstSipMsg :: sipmsg(),
      ResultSipMsg :: sipmsg().
maybe_set_to_tag(Reply, SipMsg, RSipMsg) ->
    case ersip_reply:status(Reply) of
        100 ->
            ersip_siphdr:copy_header(to, SipMsg, RSipMsg);
        _ ->
            ToTag =
                case ersip_reply:to_tag(Reply) of
                    auto ->
                        {tag, ersip_id:token(crypto:strong_rand_bytes(8))};
                    {tag, _} = Tag ->
                        Tag
                end,
            To = ersip_sipmsg:get(to, SipMsg),
            NewTo = ersip_hdr_fromto:set_tag(ToTag, To),
            set(to, NewTo, RSipMsg)
    end.

-spec parse_validate_cseq(sipmsg()) -> boolean().
parse_validate_cseq(#sipmsg{headers = #{cseq := CSeq}} = SipMsg) ->
    ReqMethod  = ersip_sipmsg:method(SipMsg),
    CSeqMethod = ersip_hdr_cseq:method(CSeq),
    %% TODO maybe we may pass this check for unknown method:
    %% ReqMethod == CSeqMethod orelse not ersip_method:is_known(ReqMethod);
    ReqMethod == CSeqMethod;
parse_validate_cseq(#sipmsg{}) ->
    true.
'''