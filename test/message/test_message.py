import pytest
from pysip.message.message import Message, MessageError, MessageType, ResponseType, RequestType, TYPE_RESPONSE, \
    TYPE_REQUEST, ITEM_RURI, ITEM_TYPE, ITEM_METHOD, ITEM_STATUS, ITEM_REASON, ITEM_BODY
from pysip.message.method import Method, INVITE, REGISTER, ACK


@pytest.mark.parametrize('msg_type, type_name', [(ResponseType, TYPE_RESPONSE),
                                      (RequestType, TYPE_REQUEST)])
def test_set_type(msg_type, type_name):
    msg = Message()
    msg.type = msg_type()
    assert msg.type == type_name


@pytest.mark.parametrize('status', [100, 199, 200, 299, 300, 399, 400, 499, 500, 599])
def test_set_status(status):
    msg = Message()
    msg.type = ResponseType()
    assert msg.status is None
    msg.status = status
    assert msg.status == status


@pytest.mark.parametrize('reason', ['OK', 'Ringing', 'Temorary Failure'])
def test_set_reason(reason):
    msg = Message()
    msg.type = ResponseType()
    assert msg.reason is None
    msg.reason = reason
    assert msg.reason == reason


@pytest.mark.parametrize('method', [INVITE, REGISTER, ACK])
def test_set_method(method):
    msg = Message()
    msg.type = RequestType()
    assert msg.method is None
    msg.method = method
    assert msg.method == method


@pytest.mark.parametrize('ruri', ["sip:a@b", "sip:a@b:5060", "sip:a@b;transport=tls"])
def test_set_ruri(ruri):
    msg = Message()
    msg.type = RequestType()
    assert msg.ruri is None
    msg.ruri = ruri
    assert msg.ruri == ruri


def test_reset_type():
    msg = Message()
    msg.type = RequestType()
    assert msg.type == TYPE_REQUEST
    msg.type = ResponseType()
    assert msg.type == TYPE_RESPONSE
    msg.type = RequestType()
    assert msg.type == TYPE_REQUEST


def test_get_header():
    msg = Message()
    msg.add('Some-Header', "1")
    msg.add('Some-Header', 'a,b')
    hdr = msg.get('SOME-HEADER')
    assert hdr.values == ['1', 'a,b']


def test_message_serialize_request():
    msg = Message()
    msg.type = RequestType()
    msg.method = INVITE
    msg.ruri = "sip:alice@example.com"
    msg.set('Via', 'SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds')
    msg.set('From', 'sip:bob@biloxi.com')
    msg.set('To', 'sip:alice@example.com')
    msg.set('Max-Forwards', '70')
    msg.set('CSeq', '1 INVITE')
    msg.set('Call-Id', 'some-call-id')
    exp_msg = '\r\n'.join(['INVITE sip:alice@example.com SIP/2.0',
                           'Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds',
                           'To: sip:alice@example.com',
                           'From: sip:bob@biloxi.com',
                           'Call-Id: some-call-id',
                           'CSeq: 1 INVITE',
                           'Max-Forwards: 70']) + '\r\n\r\n'
    assert exp_msg == msg.serialize()


def test_message_serialize_response():
    msg = Message()
    msg.type = ResponseType()
    msg.status = 200
    msg.reason = 'OK'
    msg.set('Via', ["SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1",
                    "SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"])
    msg.set('From', 'sip:bob@biloxi.com')
    msg.set('To', 'sip:alice@example.com')
    msg.set('CSeq', '1 INVITE')
    msg.set('Call-Id', 'some-call-id')
    exp_msg = '\r\n'.join(['SIP/2.0 200 OK',
                           'Via: SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1',
                           'Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds',
                           'To: sip:alice@example.com',
                           'From: sip:bob@biloxi.com',
                           'Call-Id: some-call-id',
                           'CSeq: 1 INVITE']) + '\r\n\r\n'
    assert exp_msg == msg.serialize()


def test_message_get_headers():
    msg = Message()
    msg.type = RequestType()
    msg.method = INVITE
    msg.ruri = 'sip:alice@example.com'
    msg.set('Via', 'SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds')
    msg.set('From', 'sip:bob@biloxi.com')
    msg.set('To', 'sip:alice@example.com')
    msg.set('Max-Forwards', '70')
    msg.set('CSeq', '1 INVITE')
    msg.set('Call-Id', 'some-call-id')
    assert msg.get('Via').values == ['SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds']
    assert msg.get('From').values == ['sip:bob@biloxi.com']
    assert msg.get('To').values == ['sip:alice@example.com']
    assert msg.get('Max-Forwards').values == ['70']
    assert msg.get('CSeq').values == ['1 INVITE']
    assert msg.get('Call-Id').values == ['some-call-id']


def test_message_invalid_method():
    msg = Message()
    msg.type = RequestType()
    with pytest.raises(MessageError):
        msg.method = 123


def test_message_clear_headers():
    msg = Message()
    msg.type = RequestType()
    msg.method = INVITE
    msg.ruri = 'sip:alice@example.com'
    msg.set('Via', 'SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds')
    msg.set('From', 'sip:bob@biloxi.com')
    msg.set('To', 'sip:alice@example.com')
    msg.set('Max-Forwards', '70')
    msg.set('CSeq', '1 INVITE')
    msg.set('Call-Id', 'some-call-id')
    msg.clear_header()
    assert [] == msg.headers
