from pysip.message.sipmsg import SipMsg, parse, SIPMessageParseError, ReplyOptions, Reply
import pysip.message.method as sip_method
import pytest
from twisted.protocols.sip import MessagesParser


callid = b'a84b4c76e66710@pc33.atlanta.com'
req_msg = b"INVITE sip:bob@biloxi.com SIP/2.0" \
          b"\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds" \
          b"\r\nMax-Forwards: 70" \
          b"\r\nTo: Bob <sip:bob@biloxi.com>" \
          b"\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774" \
          b"\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com" \
          b"\r\nCSeq: 314159 INVITE" \
          b"\r\nContact: <sip:alice@pc33.atlanta.com>" \
          b"\r\nContent-Type: application/sdp" \
          b"\r\nContent-Length: 4" \
          b"\r\n\r\nTest"

resp_msg = b"SIP/2.0 200 OK" \
           b"\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds" \
           b"\r\nMax-Forwards: 70" \
           b"\r\nTo: Bob <sip:bob@biloxi.com>" \
           b"\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774" \
           b"\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com" \
           b"\r\nCSeq: 314159 INVITE" \
           b"\r\nContact: <sip:alice@pc33.atlanta.com>" \
           b"\r\nContent-Type: application/sdp" \
           b"\r\nContent-Length: 4" \
           b"\r\n\r\nTest"


def default_sip_msg():
    msg = b"INVITE sip:bob@biloxi.com SIP/2.0"\
          b"\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"\
          b"\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com"\
          b"\r\nMax-Forwards: 70"\
          b"\r\nTo: Bob <sip:bob@biloxi.com>"\
          b"\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774"\
          b"\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com"\
          b"\r\nCSeq: 314159 INVITE"\
          b"\r\nContact: <sip:alice@pc33.atlanta.com>"\
          b"\r\nContent-Type: application/sdp"\
          b"\r\nContent-Length: 4"\
          b"\r\n\r\nTest"
    return parse(msg)


def test_clear_headers():
    msg = SipMsg()
    msg.method = sip_method.invite()
    msg.ruri = "sip:alice@example.com"
    msg.add_header("Via", "SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds")
    msg.add_header("From", "sip:bob@biloxi.com")
    msg.add_header("To", "sip:alice@example.com")
    msg.add_header("Max-Forwards", "70")
    msg.add_header("CSeq", "1 INVITE")
    msg.add_header("Call-Id", "some-call-id")
    msg.clear_headers()
    assert not msg.headers


@pytest.mark.not_ready
def test_request_parse():

    parsed = parse(req_msg)
    assert callid.decode('utf8') == parsed.callid
    assert "70" == parsed.maxforwards
    '''
    From = ersip_sipmsg:get(from, SipMsg),
    From = ersip_sipmsg:from(SipMsg),
    To   = ersip_sipmsg:get(to, SipMsg),
    To   = ersip_sipmsg:to(SipMsg),
    ?assertEqual({tag, <<"1928301774">>},       ersip_hdr_fromto:tag(From)),
    ?assertEqual({display_name, [<<"Bob">>]}, ersip_hdr_fromto:display_name(To)),
    Via  = ersip_sipmsg:get(topmost_via, SipMsg),
    Via  = ersip_sipmsg:topmost_via(SipMsg),
    ?assertEqual({sent_by, {hostname, <<"pc33.atlanta.com">>}, 5060},
                 ersip_hdr_via:sent_by(Via)),
    CSeq = ersip_sipmsg:get(cseq, SipMsg),
    CSeq = ersip_sipmsg:cseq(SipMsg),
    ?assertEqual(314159, ersip_hdr_cseq:number(CSeq)),
    ?assertEqual(ersip_method:invite(), ersip_hdr_cseq:method(CSeq)),
    ok.
    '''


@pytest.mark.not_ready
def test_parse_response():
    parsed = parse(resp_msg)
    assert callid.decode('utf8') == parsed.callid
    assert "70" == parsed.maxforwards

    '''
    From = ersip_sipmsg:get(from, SipMsg),
    To   = ersip_sipmsg:get(to, SipMsg),
    ?assertEqual({tag, <<"1928301774">>},       ersip_hdr_fromto:tag(From)),
    ?assertEqual({display_name, [<<"Bob">>]}, ersip_hdr_fromto:display_name(To)),
    Via  = ersip_sipmsg:get(topmost_via, SipMsg),
    ?assertEqual({sent_by, {hostname, <<"pc33.atlanta.com">>}, 5060},
                 ersip_hdr_via:sent_by(Via)),
    '''


def test_parse_response_error():
    err_msg = b"SIP/2.0 200 OK" \
              b"\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds" \
              b"\r\nMax-Forwards: 70" \
              b"\r\nTo: Bob <sip:bob@biloxi.com>" \
              b"\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774" \
              b"\r\nCall-ID: " + callid + b'' \
              b"\r\nCSeq: x" \
              b"\r\nContact: <sip:alice@pc33.atlanta.com>" \
              b"\r\nContent-Type: application/sdp" \
              b"\r\nContent-Length: 4" \
              b"\r\n\r\nTest"
    with pytest.raises(SIPMessageParseError):
        parsed = parse(err_msg)
        print(parsed.message)


@pytest.mark.not_ready
def test_parse_request_without_body():
    req_msg = b"INVITE sip:bob@biloxi.com SIP/2.0" \
              b"\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds" \
              b"\r\nMax-Forwards: 70" \
              b"\r\nTo: Bob <sip:bob@biloxi.com>" \
              b"\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774" \
              b"\r\nCall-ID: " + callid + b'' \
              b"\r\nCSeq: 314159 INVITE" \
              b"\r\nContact: <sip:alice@pc33.atlanta.com>" \
              b"\r\nContent-Length: 0" \
              b"\r\n\r\n"

    parsed = parse(req_msg)
    assert callid.decode('utf8') == parsed.callid
    assert "70" == parsed.maxforwards
    '''
    From = ersip_sipmsg:get(from, SipMsg),
    To   = ersip_sipmsg:get(to, SipMsg),
    ?assertEqual({tag, <<"1928301774">>},       ersip_hdr_fromto:tag(From)),
    ?assertEqual({display_name, [<<"Bob">>]}, ersip_hdr_fromto:display_name(To)),
    Via  = ersip_sipmsg:get(topmost_via, SipMsg),
    ?assertEqual({sent_by, {hostname, <<"pc33.atlanta.com">>}, 5060},
                 ersip_hdr_via:sent_by(Via)),
    ?assertError({error, _}, ersip_sipmsg:get(content_type, SipMsg)),
    ok.
    '''


@pytest.mark.not_ready
def testy_parse_request_with_body_no_content_type():
    req_msg = b"INVITE sip:bob@biloxi.com SIP/2.0" \
            b"\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds" \
            b"\r\nMax-Forwards: 70" \
            b"\r\nTo: Bob <sip:bob@biloxi.com>" \
            b"\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774" \
            b"\r\nCall-ID: " + callid + b'' \
            b"\r\nCSeq: 314159 INVITE" \
            b"\r\nContact: <sip:alice@pc33.atlanta.com>" \
            b"\r\nContent-Length: 4" \
            b"\r\n\r\nTest"
    with pytest.raises(SIPMessageParseError):
        parsed = parse(req_msg)
    '''
    ?assertEqual({error,{header_error,
                         {content_type,
                          {no_required_header,<<"Content-Type">>}}}},
                 ersip_sipmsg:parse(Msg, all)),
    ok.
    '''


def test_parse_request_with_invalid_ruri():
    req_msg = b"INVITE bob@biloxi.com SIP/2.0"\
            b"\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"\
            b"\r\nMax-Forwards: 70"\
            b"\r\nTo: Bob <sip:bob@biloxi.com>"\
            b"\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774"\
            b"\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com",\
            b"\r\nCSeq: 314159 INVITE"\
            b"\r\nContact: <sip:alice@pc33.atlanta.com>"\
            b"\r\nContent-Length: 4"\
            b"\r\n\r\nTest"
    with pytest.raises(SIPMessageParseError):
        parsed = parse(req_msg)
    '''
    ?assertMatch({error,{invalid_ruri, _}}, ersip_sipmsg:parse(Msg, all)),
    ok.
    '''


@pytest.mark.not_ready
def test_parse_no_required_field():
    req_msg = b"INVITE sip:bob@biloxi.com SIP/2.0"\
            b"\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"\
            b"\r\nMax-Forwards: 70"\
            b"\r\nTo: Bob <sip:bob@biloxi.com>"\
            b"\r\nCall-ID: " + callid + b''\
            b"\r\nCSeq: 314159 INVITE"\
            b"\r\nContact: <sip:alice@pc33.atlanta.com>"\
            b"\r\nContent-Type: application/sdp"\
            b"\r\nContent-Length: 4"\
            b"\r\n\r\nTest"
    with pytest.raises(SIPMessageParseError):
        parsed = parse(req_msg)


@pytest.mark.not_ready
def test_reply():
    req_msg = b"INVITE sip:bob@biloxi.com SIP/2.0"\
            b"\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"\
            b"\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com"\
            b"\r\nMax-Forwards: 70"\
            b"\r\nTo: Bob <sip:bob@biloxi.com>"\
            b"\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774"\
            b"\r\nCall-ID: " + callid + b''\
            b"\r\nCSeq: 314159 INVITE"\
            b"\r\nContact: <sip:alice@pc33.atlanta.com>"\
            b"\r\nContent-Type: application/sdp"\
            b"\r\nContent-Length: 4"\
            b"\r\n\r\nTest"
    parsed = parse(req_msg)
    reply = Reply(options=ReplyOptions(404, to_tag=b'4212312424'), message=parsed)
    assert reply.from_value == parsed.from_value
    assert reply.callid == parsed.callid
    assert reply.cseq == parsed.cseq
    assert reply.via == parsed.via
    '''
    ToReq  = ersip_sipmsg:get(to, SipMsg),
    ToResp = ersip_sipmsg:get(to, RespSipMsg),
    %% However, if the To header field in the request did not contain
    %% a tag, the URI in the To header field in the response MUST
    %% equal the URI in the To header field
    ?assertEqual(ersip_hdr_fromto:uri(ToReq), ersip_hdr_fromto:uri(ToResp)),
    %% additionally, the UAS MUST add a tag to the To header field in
    %% the response
    ?assertEqual(ToTag, ersip_hdr_fromto:tag(ToResp)),
    ok.
    '''


@pytest.mark.not_ready
def test_reply_totag_autogenerated():
    req_msg = b"INVITE sip:bob@biloxi.com SIP/2.0" \
              b"\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds" \
              b"\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com" \
              b"\r\nMax-Forwards: 70" \
              b"\r\nTo: Bob <sip:bob@biloxi.com>" \
              b"\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774" \
              b"\r\nCall-ID: " + callid + b'' \
              b"\r\nCSeq: 314159 INVITE" \
              b"\r\nContact: <sip:alice@pc33.atlanta.com>" \
              b"\r\nContent-Type: application/sdp" \
              b"\r\nContent-Length: 4" \
              b"\r\n\r\nTest"

    parsed = parse(req_msg)
    reply_opts = ReplyOptions(404)
    reply = Reply(options=reply_opts, message=parsed)
'''  reply_totag_autogenerated_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    ReplyOpts = ersip_reply:new(404),
    RespMsg = ersip_sipmsg:reply(ReplyOpts, SipMsg),
    ToTag = ersip_hdr_fromto:tag(ersip_sipmsg:get(to, RespMsg)),
    ?assert(ersip_parser_aux:check_token(ersip_hdr_fromto:assemble(ToTag))),
    ok.'''


@pytest.mark.not_ready
def test_reply_indialog():
    req_msg = b"INVITE sip:bob@biloxi.com SIP/2.0"\
            b"\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"\
            b"\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com"\
            b"\r\nMax-Forwards: 70"\
            b"\r\nTo: Bob <sip:bob@biloxi.com>;tag=1234421234"\
            b"\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774"\
            b"\r\nCall-ID: " + callid + b''\
            b"\r\nCSeq: 314159 INVITE"\
            b"\r\nContact: <sip:alice@pc33.atlanta.com>"\
            b"\r\nContent-Type: application/sdp"\
            b"\r\nContent-Length: 4"\
            b"\r\n\r\nTest"
    parsed = parse(req_msg)
    reply_opts = ReplyOptions(404, reason="My Not Found")
    reply = Reply(options=reply_opts, message=parsed)
    assert reply.from_value == parsed.from_value
    assert reply.callid == parsed.callid
    assert reply.cseq == parsed.cseq
    assert reply.via == parsed.via
    '''
    %% If a request contained a To tag in the request, the To header
    %% field in the response MUST equal that of the request.
    ?assertEqual(ersip_sipmsg:get(to, SipMsg), ersip_sipmsg:get(to, RespSipMsg)),
    ok.
    '''


@pytest.mark.not_ready
def test_reply_100_trying():
    req_msg = b"INVITE sip:bob@biloxi.com SIP/2.0"\
            b"\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"\
            b"\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com"\
            b"\r\nMax-Forwards: 70"\
            b"\r\nTo: Bob <sip:bob@biloxi.com>"\
            b"\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774"\
            b"\r\nCall-ID: " + callid + b''\
            b"\r\nCSeq: 314159 INVITE"\
            b"\r\nContact: <sip:alice@pc33.atlanta.com>"\
            b"\r\nContent-Type: application/sdp"\
            b"\r\nContent-Length: 4"\
            b"\r\n\r\nTest"
    parsed = parse(req_msg)
    reply_opts = ReplyOptions(100)
    reply = Reply(options=reply_opts, message=parsed)
    assert reply.from_value == parsed.from_value
    assert reply.callid == parsed.callid
    assert reply.cseq == parsed.cseq
    assert reply.via == parsed.via
'''     
    ToReq  = ersip_sipmsg:get(to, SipMsg),
    ToResp = ersip_sipmsg:get(to, RespSipMsg),
    ?assertEqual(ToReq, ToResp),
    ok.'''


@pytest.mark.not_ready
def test_get_parts():
    req_msg = b"INVITE sip:bob@biloxi.com SIP/2.0"\
            b"\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"\
            b"\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com"\
            b"\r\nMax-Forwards: 70"\
            b"\r\nTo: Bob <sip:bob@biloxi.com>"\
            b"\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774"\
            b"\r\nCall-ID: " + callid + b''\
            b"\r\nCSeq: 314159 INVITE"\
            b"\r\nContact: <sip:alice@pc33.atlanta.com>"\
            b"\r\nContent-Type: application/sdp"\
            b"\r\nContent-Length: 4"\
            b"\r\n\r\nTest"
    parsed = parse(req_msg)
    assert parsed.status is None
    assert parsed.reason is None
    assert parsed.method == sip_method.invite()
'''get_parts_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    ?assertEqual(undefined, ersip_sipmsg:status(SipMsg)),
    ?assertEqual(undefined, ersip_sipmsg:reason(SipMsg)),
    ?assertEqual({method, <<"INVITE">>}, ersip_sipmsg:method(SipMsg)),
    ok.'''


def test_set_ruri():
    sip_msg = default_sip_msg()
    ruri_bin = b'"sip:alice@atlanta.com"'
    assert sip_msg.ruri == 0

    '''
    SipMsg0 = default_sipmsg(),
    RURIBin = <<"sip:alice@atlanta.com">>,
    RURI = ersip_uri:make(RURIBin),
    SipMsg1 = ersip_sipmsg:set_ruri(RURI, SipMsg0),
    ?assertEqual(RURI, ersip_sipmsg:ruri(SipMsg1)),
    SipMsg2 = rebuild_sipmsg(SipMsg1),
    ?assertEqual(RURI, ersip_sipmsg:ruri(SipMsg2)),
    ok.'''