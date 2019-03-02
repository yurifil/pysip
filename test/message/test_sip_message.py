import pytest
from pysip.message.sip_message import SipMessage, SipMessageError, SipHeaderError, ALL, NotFound, SIPMessageParseError
from pysip.message.hdr import Header
from pysip.message.hdr_callid import CallIDHeader
from pysip.message.hdr_maxforwards import MaxForwardsHeader
from pysip.message.hdr_via import SentBy
from pysip.message.hdr_cseq import CSeqHeaderError
from pysip.message.method import INVITE
from pysip.message.hnames import CALLID_HEADER, MAXFORWARDS_HEADER, CONTACT_HEADER
from pysip.message.message import TYPE_REQUEST
from pysip.message.parser_aux import check_token
from pysip.uri.uri_parser import URIParseError
from pysip.uri.uri import Uri

from pysip.reply import ReplyOptions


def test_parse_request():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = SipMessage.parse(msg, ALL)
    assert sip_msg.get(CALLID_HEADER) == CallIDHeader(callid)
    assert sip_msg.callid == CallIDHeader(callid)
    assert sip_msg.get(MAXFORWARDS_HEADER) == MaxForwardsHeader('70')
    assert sip_msg.maxforwards == MaxForwardsHeader('70')
    assert sip_msg.from_header.tag == "1928301774"
    assert sip_msg.to.display_name == 'Bob'
    assert sip_msg.topmost_via.sent_by == SentBy('pc33.atlanta.com', 5060)
    assert sip_msg.cseq.number == 314159
    assert sip_msg.cseq.method == INVITE


def test_parse_request_append_all():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg_from = SipMessage.parse(msg, ['from'])
    sip_msg = SipMessage.parse(sip_msg_from, ALL)
    assert sip_msg.get(CALLID_HEADER) == CallIDHeader(callid)
    assert sip_msg.callid == CallIDHeader(callid)
    assert sip_msg.get(MAXFORWARDS_HEADER) == MaxForwardsHeader('70')
    assert sip_msg.maxforwards == MaxForwardsHeader('70')
    assert sip_msg.from_header.tag == "1928301774"
    assert sip_msg.to.display_name == 'Bob'
    assert sip_msg.topmost_via.sent_by == SentBy('pc33.atlanta.com', 5060)
    assert sip_msg.cseq.number == 314159
    assert sip_msg.cseq.method == INVITE


def test_parse_response():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'SIP/2.0 200 OK' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = SipMessage.parse(msg, ALL)
    assert sip_msg.callid == CallIDHeader(callid)
    assert sip_msg.maxforwards == MaxForwardsHeader('70')
    assert sip_msg.from_header.tag == "1928301774"
    assert sip_msg.to.display_name == 'Bob'
    assert sip_msg.topmost_via.sent_by == SentBy('pc33.atlanta.com', 5060)


def test_parse_response_error():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'SIP/2.0 200 OK' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: x' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    with pytest.raises(CSeqHeaderError):
        SipMessage.parse(msg, ALL)


def test_parse_request_without_body():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Length: 0' + \
          '\r\n\r\n'
    sip_msg = SipMessage.parse(msg, ALL)
    assert sip_msg.get(CALLID_HEADER) == CallIDHeader(callid)
    assert sip_msg.maxforwards == MaxForwardsHeader('70')
    assert sip_msg.from_header.tag == "1928301774"
    assert sip_msg.to.display_name == 'Bob'
    assert sip_msg.topmost_via.sent_by == SentBy('pc33.atlanta.com', 5060)
    with pytest.raises(SipMessageError):
        sip_msg.get('content-type')


def test_parse_request_with_body_no_content_type():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    with pytest.raises(SipHeaderError):
        SipMessage.parse(msg, ALL)


def test_parse_request_with_invalid_ruri():
    # ruri has no scheme
    msg = 'INVITE bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    with pytest.raises(URIParseError):
        SipMessage.parse(msg, ALL)


def test_parse_on_demand():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = SipMessage.parse(msg, [])
    assert sip_msg.headers == {}
    assert sip_msg.get(CALLID_HEADER) == CallIDHeader(callid)
    assert sip_msg.maxforwards == MaxForwardsHeader('70')
    assert sip_msg.from_header.tag == "1928301774"
    assert sip_msg.to.display_name == 'Bob'
    assert sip_msg.topmost_via.sent_by == SentBy('pc33.atlanta.com', 5060)


def test_parse_no_required_field():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    with pytest.raises(SipHeaderError):
        SipMessage.parse(msg, ALL)


def test_parse_on_demand_parse_error():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: x' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = SipMessage.parse(msg, [])
    assert sip_msg.callid == CallIDHeader(callid)
    with pytest.raises(SipHeaderError):
        sip_msg.get('content-type')


def test_get_parts():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = SipMessage.parse(msg, ALL)
    assert sip_msg.status is None
    assert sip_msg.reason is None
    assert sip_msg.method == INVITE


def rebuild(message):
    msg_str = message.serialize()
    print(msg_str)
    sip_msg = SipMessage.parse(msg_str, ALL)
    print(sip_msg)
    return sip_msg


def test_set_ruri():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = SipMessage.parse(msg, ALL)
    uri = Uri('sip:alice@atlanta.com')
    sip_msg.ruri = uri
    assert sip_msg.ruri == uri
    sip_msg2 = rebuild(sip_msg)
    assert sip_msg2.ruri == uri


def test_userdata():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = SipMessage.parse(msg, ALL)
    sip_msg.user_data = 'my_data'
    assert sip_msg.user_data == 'my_data'
    sip_msg.clear_user_data()
    with pytest.raises(SipMessageError):
        return sip_msg.user_data


def test_raw_ruri_manipulations():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = SipMessage.parse(msg, ALL)
    ruri = sip_msg.ruri
    sip_msg.ruri = ruri
    raw_msg = sip_msg.raw_message
    SipMessage.parse(raw_msg, ALL)


def test_set_raw_header_not_parsed():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
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
    sip_msg = SipMessage.parse(msg, [])
    new_contact_str = 'Alice <sip:alice@pc34.atlanta.com>'
    contact_hdr = Header(CONTACT_HEADER)
    contact_hdr.add_value(new_contact_str)
    sip_msg.set_raw_header(contact_hdr)
    parsed_contact_list = sip_msg.get(CONTACT_HEADER)
    new_alice_contact = parsed_contact_list.values[0]
    assert new_alice_contact.assemble() == new_contact_str


def test_set_raw_header_parsed():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
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
    sip_msg = SipMessage.parse(msg, [CONTACT_HEADER])
    new_contact_str = 'Alice <sip:alice@pc34.atlanta.com>'
    contact_hdr = Header(CONTACT_HEADER)
    contact_hdr.add_value(new_contact_str)
    sip_msg.set_raw_header(contact_hdr)
    parsed_contact_list = sip_msg.get(CONTACT_HEADER)
    new_alice_contact = parsed_contact_list.values[0]
    assert new_alice_contact.assemble() == new_contact_str


def test_remove_header():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Length: 0' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\n\r\n'
    sip_msg = SipMessage.parse(msg, ALL)
    assert not isinstance(sip_msg.find('content-type'), NotFound)
    sip_msg.remove('content-type')
    assert isinstance(sip_msg.find('content-type'), NotFound)


def test_remove_custom_header():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Length: 0' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nMyCustomHeader: Value' + \
          '\r\n\r\n'
    sip_msg = SipMessage.parse(msg, ALL)
    raw_header = sip_msg.raw_header('MyCustomHeader')
    assert len(raw_header.values) != 0
    sip_msg.remove('MyCustomHeader')
    raw_header = sip_msg.raw_header('MyCustomHeader')
    assert len(raw_header.values) == 0


def test_remove_body():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = SipMessage.parse(msg, ALL)
    sip_msg.remove_body()
    assert not sip_msg.has_body()
    sip_msg.remove('Content-Length')
    rebuilt_sip_msg = rebuild(sip_msg)
    assert not rebuilt_sip_msg.has_body()


def test_cannot_set_status_of_request():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = SipMessage.parse(msg, ALL)
    assert sip_msg.type == TYPE_REQUEST
    with pytest.raises(SipMessageError):
        sip_msg.status = 200


@pytest.mark.parametrize('raw', ('@',
                                 'SIP/2.0 200 OK',
                                 'INVITE sip:alice@atlanta.com SIP/2.0\r\n'))
def test_truncated_message_error(raw):
    with pytest.raises(SIPMessageParseError) as excinfo:
        SipMessage.parse(raw, ALL)
    assert 'truncated' in str(excinfo.value)


def test_generic_parse_error():
    with pytest.raises(SIPMessageParseError):
        SipMessage.parse('INVITE sip:alice@atlanta.com SIP/2.0\r\nContent-Length: 100\r\n\r\n', ALL)


def test_reply():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = SipMessage.parse(msg, ALL)
    to_tag = '4212312424'
    reply_opts = ReplyOptions(404, to_tag=to_tag)
    respond = sip_msg.reply(reply_opts)
    assert respond.from_header == sip_msg.from_header
    assert respond.callid == sip_msg.callid
    assert respond.cseq == sip_msg.cseq
    assert respond.raw_header('via') == sip_msg.raw_header('via')
    assert respond.to.tag == sip_msg.to.tag
    assert respond.to.uri == sip_msg.to.uri


def test_autogenerated_totag():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = SipMessage.parse(msg, ALL)
    reply_opts = ReplyOptions(404)
    respond = sip_msg.reply(reply_opts)
    assert check_token(respond.to.tag)


def test_reply_indialog():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = SipMessage.parse(msg, ALL)
    reply_opts = ReplyOptions(404, reason='My Not Found')
    reply = sip_msg.reply(reply_opts)
    assert sip_msg.from_header == reply.from_header
    assert sip_msg.callid == reply.callid
    assert sip_msg.cseq == reply.cseq
    assert reply.raw_header('via') == sip_msg.raw_header('via')
    assert reply.to.tag == sip_msg.to.tag
    assert reply.to.uri == sip_msg.to.uri


def test_reply_100_trying():
    callid = 'a84b4c76e66710@pc33.atlanta.com'
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: ' + callid + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = SipMessage.parse(msg, ALL)
    reply_opts = ReplyOptions(100)
    respond = sip_msg.reply(reply_opts)
    assert respond.from_header == sip_msg.from_header
    assert respond.callid == sip_msg.callid
    assert respond.cseq == sip_msg.cseq
    assert respond.raw_header('via') == sip_msg.raw_header('via')
    assert respond.to.tag == sip_msg.to.tag
    assert respond.to.uri == sip_msg.to.uri
