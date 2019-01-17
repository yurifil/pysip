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


def test_reply_test():
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
    assert reply



    %% The Via header field values in the response MUST equal the Via
    %% header field values in the request and MUST maintain the same
    %% ordering.
    ?assertEqual(ersip_sipmsg:raw_header(<<"via">>, SipMsg), ersip_sipmsg:raw_header(<<"via">>, RespSipMsg)),
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