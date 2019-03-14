from pysip.proxy.proxy_stateless import StatelessProxy, StatelessProxyError, ProcessResponseDrop, ProcessResponseForward
from pysip.message.sip_message import SipMessage, ALL
from pysip.uri.transport import UDP, TLS, Transport
import pytest


def stateless_branch(string):
    return StatelessProxy.branch(SipMessage.parse(string, ALL))


def three_msg_check(eq1, eq2, ne):
    b1 = stateless_branch(eq1)
    b2 = stateless_branch(eq2)
    b3 = stateless_branch(ne)
    for b in (b1, b2, b3):
        assert b.is_rfc3261()
    assert b1 == b2
    assert b1 != b3


def reply_via_conn(sip_msg, reply, conn):
    req_sip_raw_msg = sip_msg.raw_message
    branch = StatelessProxy.branch(sip_msg)

'''
three_message_check(Eq1, Eq2, Neq) ->
    Branch1 = stateless_branch(Eq1),
    Branch2 = stateless_branch(Eq2),
    Branch3 = stateless_branch(Neq),
    [?assert(ersip_branch:is_rfc3261(B)) || B <- [Branch1, Branch2, Branch3]],
    ?assertEqual(Branch1, Branch2),
    ?assertNotEqual(Branch1, Branch3).


stateless_branch(Bin) ->
    ersip_proxy_stateless:branch(sip_message(Bin)).

raw_message(Bin) ->
    P  = ersip_parser:new_dgram(Bin),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    PMsg.

sip_message(Bin) ->
    {ok, SipMsg} = ersip_sipmsg:parse(raw_message(Bin), all),
    SipMsg.

sip_message(Bin, Headers) ->
    {ok, SipMsg} = ersip_sipmsg:parse(raw_message(Bin), Headers),
    SipMsg.

reply_via_conn(SipMsg, Reply, Conn) ->
    ReqSIPRawMsg  = ersip_sipmsg:raw_message(SipMsg),
    Branch        = ersip_proxy_stateless:branch(SipMsg),
    ReqSIPRawMsg1 = ersip_conn:add_via(ReqSIPRawMsg, Branch, Conn),

    %% Generating reply to the message:
    {ok, ReqSIPMsg1} = ersip_sipmsg:parse(ReqSIPRawMsg1, all),
    RespSIPMsg = ersip_sipmsg:reply(Reply, ReqSIPMsg1),

    %% Passing response:
    {_, [{new_response, _, _} = SE]} = ersip_conn:conn_data(ersip_sipmsg:serialize_bin(RespSIPMsg), Conn),
    SE.

del_topmost_via(RawMsg) ->
    ViaH = ersip_msg:get(<<"via">>, RawMsg),
    {ok, _Value, NewViaH} = ersip_hdr:take_topmost(ViaH),
    ersip_msg:set_header(NewViaH, RawMsg).
'''


@pytest.mark.parametrize('messages', [(#rfc3261_test
                                       #eq1:
                                       'INVITE sip:bob@biloxi.com SIP/2.0' +
                                       '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' +
                                       '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' +
                                       '\r\nMax-Forwards: 70' +
                                       '\r\nTo: Bob <sip:bob@biloxi.com>' +
                                       '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' +
                                       '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' +
                                       '\r\nCSeq: 314159 INVITE' +
                                       '\r\nContact: <sip:alice@pc33.atlanta.com>' +
                                       '\r\nContent-Type: application/sdp' +
                                       '\r\nContent-Length: 4' +
                                       '\r\n\r\nTest',
                                       #eq2:
                                       'ACK sip:bob@biloxi.com SIP/2.0' +
                                       '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' +
                                       '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' +
                                       '\r\nMax-Forwards: 70' +
                                       '\r\nTo: Bob <sip:bob@biloxi.com>' +
                                       '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' +
                                       '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' +
                                       '\r\nCSeq: 314159 ACK' +
                                       '\r\nContact: <sip:alice@pc33.atlanta.com>' +
                                       '\r\nContent-Type: application/sdp' +
                                       '\r\nContent-Length: 4' +
                                       '\r\n\r\nTest',
                                       #ne:
                                       'ACK sip:bob@biloxi.com SIP/2.0' +
                                       '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK1234' +
                                       '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' +
                                       '\r\nMax-Forwards: 70' +
                                       '\r\nTo: Bob <sip:bob@biloxi.com>' +
                                       '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' +
                                       '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' +
                                       '\r\nCSeq: 314159 INVITE' +
                                       '\r\nContact: <sip:alice@pc33.atlanta.com>' +
                                       '\r\nContent-Type: application/sdp' +
                                       '\r\nContent-Length: 4' +
                                       '\r\n\r\nTest'),
                                      (#rfc2543 test
                                       #eq1:
                                       'INVITE sip:bob@biloxi.com SIP/2.0' +
                                       '\r\nVia: SIP/2.0/UDP pc33.atlanta.com' +
                                       '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' +
                                       '\r\nMax-Forwards: 70' +
                                       '\r\nTo: Bob <sip:bob@biloxi.com>' +
                                       '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' +
                                       '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' +
                                       '\r\nCSeq: 314159 INVITE' +
                                       '\r\nContact: <sip:alice@pc33.atlanta.com>' +
                                       '\r\nContent-Type: application/sdp' +
                                       '\r\nContent-Length: 4' +
                                       '\r\n\r\nTest',
                                       #eq2:
                                       'ACK sip:bob@biloxi.com SIP/2.0' +
                                       '\r\nVia: SIP/2.0/UDP pc33.atlanta.com' +
                                       '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' +
                                       '\r\nMax-Forwards: 70' +
                                       '\r\nTo: Bob <sip:bob@biloxi.com>' +
                                       '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' +
                                       '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' +
                                       '\r\nCSeq: 314159 ACK' +
                                       '\r\nContact: <sip:alice@pc33.atlanta.com>' +
                                       '\r\nContent-Type: application/sdp' +
                                       '\r\nContent-Length: 4' +
                                       '\r\n\r\nTest',
                                       #ne:
                                       'ACK sip:bob@biloxi.com SIP/2.0' +
                                       '\r\nVia: SIP/2.0/UDP pc33.atlanta.com' +
                                       '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' +
                                       '\r\nMax-Forwards: 70' +
                                       '\r\nTo: Bob <sip:bob@biloxi.com>' +
                                       '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=other_tag' +
                                       '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' +
                                       '\r\nCSeq: 314159 ACK' +
                                       '\r\nContact: <sip:alice@pc33.atlanta.com>' +
                                       '\r\nContent-Type: application/sdp' +
                                       '\r\nContent-Length: 4' +
                                       '\r\n\r\nTest'),
                                      (#rfc2543_with_branch_test:
                                       #eq1:
                                       'INVITE sip:bob@biloxi.com SIP/2.0' +
                                       '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=rfc2543branch' +
                                       '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' +
                                       '\r\nMax-Forwards: 70' +
                                       '\r\nTo: Bob <sip:bob@biloxi.com>' +
                                       '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' +
                                       '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' +
                                       '\r\nCSeq: 314159 INVITE' +
                                       '\r\nContact: <sip:alice@pc33.atlanta.com>' +
                                       '\r\nContent-Type: application/sdp' +
                                       '\r\nContent-Length: 4' +
                                       '\r\n\r\nTest',
                                       #eq2:
                                       'ACK sip:bob@biloxi.com SIP/2.0' +
                                       '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=rfc2543branch' +
                                       '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' +
                                       '\r\nMax-Forwards: 70' +
                                       '\r\nTo: Bob <sip:bob@biloxi.com>' +
                                       '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' +
                                       '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' +
                                       '\r\nCSeq: 314159 ACK' +
                                       '\r\nContact: <sip:alice@pc33.atlanta.com>' +
                                       '\r\nContent-Type: application/sdp' +
                                       '\r\nContent-Length: 4' +
                                       '\r\n\r\nTest',
                                       #ne:
                                       'ACK sip:bob@biloxi.com SIP/2.0' +
                                       '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=rfc2543branch' +
                                       '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' +
                                       '\r\nMax-Forwards: 70' +
                                       '\r\nTo: Bob <sip:bob@biloxi.com>' +
                                       '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=canged_tag_1928301774' +
                                       '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' +
                                       '\r\nCSeq: 314159 ACK' +
                                       '\r\nContact: <sip:alice@pc33.atlanta.com>' +
                                       '\r\nContent-Type: application/sdp' +
                                       '\r\nContent-Length: 4' +
                                       '\r\n\r\nTest')])
def test_three_messages(messages):
    msg1, msg2, msg3 = messages
    three_msg_check(msg1, msg2, msg3)



def test_process_response():
    local_ip = (127, 0, 0, 2)
    remote_ip = (127, 0, 0, 1)
    udp = Transport(UDP)
    conn = Connection(local_ip, 5061, remote_ip, 5060, udp, {})
    request = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
              '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
              '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
              '\r\nMax-Forwards: 70' + \
              '\r\nTo: Bob <sip:bob@biloxi.com>' + \
              '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
              '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
              '\r\nCSeq: 314159 INVITE' + \
              '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
              '\r\nContent-Type: application/sdp' + \
              '\r\nContent-Length: 4' + \
              '\r\n\r\nTest'

'''


process_response_test() ->
    %% Connection used for 'sending' message
    LocalIP  = {127, 0, 0, 2},
    RemoteIP = {127, 0, 0, 1},
    UDP  = ersip_transport:make(udp),
    Conn = ersip_conn:new(LocalIP, 5061, RemoteIP, 5060, UDP, #{}),

    %% Request to be sent
    Req = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,

    %% Adding branch to the message
    ReqSIPMsg     = sip_message(Req),
    {new_response, PrevVia, RawRespMsg} = reply_via_conn(ReqSIPMsg, ersip_reply:new(200), Conn),
    ProcessResult = ersip_proxy_stateless:process_response(PrevVia, RawRespMsg),

    ?assertMatch({forward, _}, ProcessResult),
    {forward, SendMessage} = ProcessResult,
    %% Message is not changed:
    ?assertEqual(ersip_msg:serialize_bin(RawRespMsg), ersip_sipmsg:serialize_bin(SendMessage)),
    ok.

process_response_no_forward_via_test() ->
    %% Connection used for 'sending' message
    LocalIP  = {127, 0, 0, 2},
    RemoteIP = {127, 0, 0, 1},
    UDP  = ersip_transport:make(udp),
    Conn = ersip_conn:new(LocalIP, 5061, RemoteIP, 5060, UDP, #{}),

    %% Request to be sent
    Req = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,

    %% Adding branch to the message
    ReqSIPMsg     = sip_message(Req, []),
    {new_response, _, RawRespMsg} = reply_via_conn(ReqSIPMsg, ersip_reply:new(200), Conn),
    PrevVia = ersip_hdr_via:new(ersip_host:make(<<"127.0.0.2">>), 5060, ersip_transport:make(udp)),
    ProcessResult = ersip_proxy_stateless:process_response(PrevVia, RawRespMsg),
    %% Action is drop because unmatched via
    ?assertMatch({drop, via_no_branch}, ProcessResult),
    ok.

process_response_via_not_match_test() ->
    %% Connection used for 'sending' message
    LocalIP  = {127, 0, 0, 2},
    RemoteIP = {127, 0, 0, 1},
    UDP  = ersip_transport:make(udp),
    Conn = ersip_conn:new(LocalIP, 5061, RemoteIP, 5060, UDP, #{}),

    %% Request to be sent
    Req = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,

    %% Adding branch to the message
    ReqSIPMsg     = sip_message(Req, []),
    {new_response, PrevVia, RawRespMsg} = reply_via_conn(ReqSIPMsg, ersip_reply:new(200), Conn),
    %% Drop via as if we received message with topmost via only (for
    %% example SIP https://freeswitch.org/jira/browse/FS-11128
    NoViaMsg = del_topmost_via(RawRespMsg),
    ProcessResult = ersip_proxy_stateless:process_response(PrevVia, NoViaMsg),

    %% Action is drop because cannot forward message to without via
    ?assertMatch({drop, no_more_via}, ProcessResult),
    ok.


%%%===================================================================
%%% Helpers
%%%===================================================================



'''
