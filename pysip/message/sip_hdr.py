from pysip.message.hnames import ALL_KNOWN_HEADERS
from pysip.message.hdr import Header, HeaderError, BaseSipHeader
from pysip.message.hnames import make_key
from pysip.message.sip_message import SipMessage
from pysip.message.message import TYPE_REQUEST, TYPE_RESPONSE
from pysip import PySIPException
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


REQUIRED_REQUESTS = 'requests'
REQUIRED_WITH_BODY = 'with_body'
REQUIRED_ALL = 'all'
REQUIRED_OPTIONAL = 'optional'


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


def all_known_headers():
    return ALL_KNOWN_HEADERS


def copy_header(header, src_msg, dst_msg):
    pass


def copy_headers(header_list, src_msg, dst_msg):
    pass


def set_header(header, value, msg):
    pass


def set_raw_header(header, msg):
    pass


def remove_header(header, msg):
    pass


def parse_header_by_descr(descr, header):
    pass


def copy_raw_header(header, src_msg, dst_msg):
    pass


def parse_header(header, msg):
    descr = header_descr(header)
    hdr = get_header(header, descr, msg)
    if isinstance(hdr, NoHeader):
        return hdr
    if isinstance(hdr, Header):
        return descr.parse_header(hdr)
    raise SipHeaderError(f'Cannot parse header {header}: header should be of type Header not {type(header)}')


def get_header(header, descr, msg):
    """

    Args:
        header (str): header name.
        descr (:obj:Description): instance of pysip.message.siphdr.Description class.
        msg (:obj:SipMessage): instance of pysip.message.sipmsg.SipMessage class.

    Returns:

    """
    header_key = make_key(header)
    header_obj = msg.raw_message.get(header_key)
    required = is_required(msg, descr.required)
    if header_obj.is_empty():
        if required:
            raise SipHeaderError(f'Cannot get required header {header}: no header.')
        return None
    return header_obj


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
        elif msg.has_body and required == REQUIRED_WITH_BODY:
            return True
        else:
            return False
    elif isinstance(msg, SipMessage):
        return is_required(RequiredEssentials(msg), required)
    else:
        raise SipHeaderError(f'Cannot decide if {msg} is required: msg should be of type SipMessage or '
                             f'RequiredEssentials, not {type(msg)}')


def header_descr(header):
    if header == 'from' or header == 'to':
        return Description(required=REQUIRED_ALL, header_class=FromToHeader)
    elif header == 'cseq':
        return Description(required=REQUIRED_ALL, header_class=CSeqHeader)
    elif header == 'callid':
        return Description(required=REQUIRED_ALL, header_class=CallIDHeader)
    elif header == 'maxforwards':
        return Description(required=REQUIRED_OPTIONAL, header_class=MaxForwardsHeader)
    elif header == 'topmost_via':
        return Description(required=REQUIRED_REQUESTS, header_class=ViaHeader)
    elif header == 'content_type':
        return Description(required=REQUIRED_WITH_BODY, header_class=ContentTypeHeader)
    elif header == 'route' or header == 'record_route':
        return Description(required=REQUIRED_OPTIONAL, header_class=RouteHeader)
    elif header == 'allow':
        return Description(required=REQUIRED_OPTIONAL, header_class=AllowHeader)
    elif header in ('supported', 'unsupported', 'require', 'proxy_require'):
        return Description(required=REQUIRED_OPTIONAL, header_class=OptTagListHeader)
    elif header == 'contact':
        return Description(required=REQUIRED_OPTIONAL, header_class=ContactHeaderList)
    elif header == 'expires' or header == 'minexpires':
        return Description(required=REQUIRED_OPTIONAL, header_class=ExpiresHeader)
    elif header == 'date':
        return Description(required=REQUIRED_OPTIONAL, header_class=DateHeader)

'''

-spec required_essentials(ersip_sipmsg:sipmsg()) -> required_essentials().
required_essentials(SipMsg) ->
    Type = ersip_sipmsg:type(SipMsg),
    #required_essentials{
       type     = Type,
       method   = ersip_sipmsg:method(SipMsg),
       status   = ersip_sipmsg:status(SipMsg),
       has_body = ersip_sipmsg:has_body(SipMsg)
      }.

-spec is_required(ersip_sipmsg:sipmsg() | required_essentials(), header_required()) -> boolean().
is_required(_, all) ->
    true;
is_required(_, optional) ->
    false;
is_required(#required_essentials{type = request}, requests) ->
    true;
is_required(#required_essentials{has_body = true}, with_body) ->
    true;
is_required(#required_essentials{}, _) ->
    false;
is_required(SipMsg, R) ->
    is_required(required_essentials(SipMsg), R).


-spec get_header(known_header(), descr(), ersip_sipmsg:sipmsg()) -> Result when
      Result :: {ok, ersip_hdr:header()}
              | {ok, no_header}
              | {error, {no_required_header, binary()}}.
get_header(HdrAtom, #descr{} = D, SipMsg) ->
    HdrKey = ersip_hnames:make_key(HdrAtom),
    Hdr = ersip_msg:get(HdrKey, ersip_sipmsg:raw_message(SipMsg)),
    Required = is_required(SipMsg, D#descr.required),
    case ersip_hdr:is_empty(Hdr) of
        true ->
            case Required of
                true ->
                    {error, {no_required_header, ersip_hnames:print_form(HdrKey)}};
                false ->
                    {ok, no_header}
            end;
        false ->
            {ok, Hdr}
    end.

parse_header_by_descr(#descr{parse_fun = F}, Hdr) ->
    F(Hdr).

-spec parse_header(known_header(), ersip_sipmsg:sipmsg()) -> ValueOrError when
      ValueOrError :: {ok, term()}
                    | {error, term()}.
parse_header(HdrAtom, Msg) when is_atom(HdrAtom) ->
    Descr = header_descr(HdrAtom),
    case get_header(HdrAtom, Descr, Msg) of
        {ok, no_header} ->
            {ok, no_header};
        {ok, Hdr} ->
            parse_header_by_descr(Descr, Hdr);
        {error, _} = Error ->
            Error
    end.

%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Headers Helpers
%%

-module(ersip_siphdr).

%%%===================================================================
%%% Types
%%%===================================================================


-record(descr, {required     :: header_required(),
                parse_fun    :: parse_fun_type(),
                assemble_fun :: undefined | assemble_fun_type()
               }).
-type descr() :: #descr{}.

-type parse_fun_type() :: fun((ersip_hdr:header()) -> parse_fun_result()).
-type parse_fun_result() :: {ok, Value :: term()}
                          | {error, Reason :: term()}.
-type assemble_fun_type() :: fun((Name :: binary(), Value :: term()) -> ersip_hdr:header()).
-type known_header() :: ersip_hnames:known_header().

%%%===================================================================
%%% API
%%%===================================================================

all_known_headers() ->
    ersip_hnames:all_known_headers().

-spec copy_header(Header, SrcSipMsg, DstSipMsg) -> NewDstSipMsg when
      Header       :: ersip_hnames:name_forms(),
      SrcSipMsg    :: ersip_sipmsg:sipmsg(),
      DstSipMsg    :: ersip_sipmsg:sipmsg(),
      NewDstSipMsg :: ersip_sipmsg:sipmsg().
copy_header(HdrAtom, SrcMsg, DstMsg0) when is_atom(HdrAtom) ->
    DstMsg1 =
        case maps:find(HdrAtom, ersip_sipmsg:headers(SrcMsg)) of
            {ok, Value} ->
                DstHeaders0 = ersip_sipmsg:headers(DstMsg0),
                ersip_sipmsg:set_headers(DstHeaders0#{HdrAtom => Value}, DstMsg0);
            error->
                DstMsg0
        end,
    copy_raw_header(HdrAtom, SrcMsg, DstMsg1);
copy_header(HdrName, SrcMsg, DstMsg) when is_binary(HdrName) ->
    HdrKey = ersip_hnames:make_key(HdrName),
    copy_header(HdrKey, SrcMsg, DstMsg);
copy_header({hdr_key, _} = HdrKey, SrcMsg, DstMsg) ->
    case ersip_hnames:known_header_form(HdrKey) of
        {ok, HdrAtom} ->
            copy_header(HdrAtom, SrcMsg, DstMsg);
        not_found ->
            copy_raw_header(HdrKey, SrcMsg, DstMsg)
    end.

-spec copy_headers(HeaderList, SrcSipMsg, DstSipMsg) -> NewDstSipMsg when
      HeaderList   :: [known_header() | binary()],
      SrcSipMsg    :: ersip_sipmsg:sipmsg(),
      DstSipMsg    :: ersip_sipmsg:sipmsg(),
      NewDstSipMsg :: ersip_sipmsg:sipmsg().
copy_headers(HeaderList, SrcSipMsg, DstSipMsg) ->
    lists:foldl(fun(Header, CurMsg) ->
                        copy_header(Header, SrcSipMsg, CurMsg)
                end,
                DstSipMsg,
                HeaderList).

-spec set_header(known_header(), Value :: term(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
set_header(Header, Value, SipMsg) when is_atom(Header) ->
    #descr{assemble_fun = AssembleF} = header_descr(Header),
    PrintName  = ersip_hnames:print_form(Header),
    OldHeaders = ersip_sipmsg:headers(SipMsg),
    OldRawMsg  = ersip_sipmsg:raw_message(SipMsg),

    RawHdr     = AssembleF(PrintName, Value),
    IsDeleted  = ersip_hdr:is_empty(RawHdr),
    NewHeaders =
        case IsDeleted of
            true ->
                maps:remove(Header, OldHeaders);
            false ->
                OldHeaders#{Header => Value}
        end,
    NewRawMsg  =
        case IsDeleted of
            true ->
                ersip_msg:del_header(RawHdr, OldRawMsg);
            false ->
                ersip_msg:set_header(RawHdr, OldRawMsg)
        end,

    SipMsg1    = ersip_sipmsg:set_headers(NewHeaders, SipMsg),
    SipMsg2    = ersip_sipmsg:set_raw_message(NewRawMsg, SipMsg1),
    SipMsg2.

%% @doc Set header to specified value. If this value is already parsed
%% then also updates parsed cached value.
-spec set_raw_header(ersip_hdr:header(), ersip_sipmsg:sipmsg()) -> {ok, ersip_sipmsg:sipmsg()} | {error, term()}.
set_raw_header(RawHdr, SipMsg0) ->
    NewRawMsg = ersip_msg:set_header(RawHdr, ersip_sipmsg:raw_message(SipMsg0)),
    SipMsg = ersip_sipmsg:set_raw_message(NewRawMsg, SipMsg0),
    case ersip_hnames:known_header_form(ersip_hdr:make_key(RawHdr)) of
        not_found ->
            %% For unknown headers: set only raw header and that is it.
            {ok, SipMsg};
        {ok, HdrAtom} ->
            %% For known headers: set only raw header and try to parse
            %% it if it is already parsed
            ParsedHeaders = ersip_sipmsg:headers(SipMsg),
            case maps:find(HdrAtom, ParsedHeaders) of
                {ok, _} ->
                    ParsedHeaders1 = maps:remove(HdrAtom, ParsedHeaders),
                    ersip_sipmsg:parse(ersip_sipmsg:set_headers(ParsedHeaders1, SipMsg), [HdrAtom]);
                error ->
                    {ok, SipMsg}
            end
    end.

-spec remove_header(ersip_hnames:name_forms(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
remove_header(Header, SipMsg) when is_atom(Header) ->
    OldHeaders = ersip_sipmsg:headers(SipMsg),
    OldRawMsg  = ersip_sipmsg:raw_message(SipMsg),

    RawHdr = ersip_hdr:new(Header),

    NewHeaders = maps:remove(Header, OldHeaders),
    NewRawMsg = ersip_msg:del_header(RawHdr, OldRawMsg),

    SipMsg1 = ersip_sipmsg:set_headers(NewHeaders, SipMsg),
    SipMsg2 = ersip_sipmsg:set_raw_message(NewRawMsg, SipMsg1),
    SipMsg2;
remove_header(HdrName, SipMsg) when is_binary(HdrName) ->
    HdrKey = ersip_hdr:make_key(HdrName),
    remove_header(HdrKey, SipMsg);
remove_header({hdr_key, _} = HKey, SipMsg) ->
    case ersip_hnames:known_header_form(HKey) of
        {ok, HdrAtom} ->
            remove_header(HdrAtom, SipMsg);
        not_found ->
            OldRawMsg = ersip_sipmsg:raw_message(SipMsg),
            NewRawMsg = ersip_msg:del_header(HKey, OldRawMsg),
            ersip_sipmsg:set_raw_message(NewRawMsg, SipMsg)
    end.

%%%===================================================================
%%% Internal implementation
%%%===================================================================


-type header_required() :: all        %% Header required for all requests/responses
                         | optional   %% Header is optional for all requests/responses
                         | with_body  %% Header required if body is not empty
                         | requests.  %% Header is required in requests

-record(required_essentials, {type     :: ersip_msg:type(),
                              method   :: ersip_method:method(),
                              status   :: undefined | ersip_status:code(),
                              has_body :: boolean()
                             }).
-type required_essentials() :: #required_essentials{}.

-spec parse_header_by_descr(descr(), ersip_hdr:header()) -> Result when
      Result :: {ok, Value :: term()}
              | {error, term()}.




-spec copy_raw_header(HeaderName, SrcSipMsg, DstSipMsg) -> NewDstSipMsg when
      HeaderName   :: ersip_hnames:name_forms(),
      SrcSipMsg    :: ersip_sipmsg:sipmsg(),
      DstSipMsg    :: ersip_sipmsg:sipmsg(),
      NewDstSipMsg :: ersip_sipmsg:sipmsg().
copy_raw_header(Header, SrcSipMsg, DstSipMsg) ->
    Key = ersip_hnames:make_key(Header),
    SrcRawMsg = ersip_sipmsg:raw_message(SrcSipMsg),
    SrcH = ersip_msg:get(Key, SrcRawMsg),
    DstRawMsg = ersip_sipmsg:raw_message(DstSipMsg),
    NewDstRawMsg = ersip_msg:set_header(SrcH, DstRawMsg),
    ersip_sipmsg:set_raw_message(NewDstRawMsg, DstSipMsg).

%%%
%%% Headers description
%%%
-spec header_descr(known_header()) -> #descr{}.



'''
