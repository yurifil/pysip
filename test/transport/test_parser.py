from pysip.transport.parser import Parser, TransportParserError
from pysip.transport.buffer import MoreDataRequired
from pysip.message.method import INVITE
import pytest


def test_basic_request():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    p = Parser.new_dgram(msg)
    parsed_msg, data = Parser.parse(p)
    assert parsed_msg.body == 'Test'


def test_request_without_body():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 0' + \
          '\r\n\r\n'
    p = Parser.new_dgram(msg)
    parsed_msg, data = Parser.parse(p)
    assert parsed_msg.method == INVITE


def test_basic_response():
    msg = 'SIP/2.0 200 OK' + \
          '\r\nVia: SIP/2.0/UDP server10.biloxi.com' + \
          '\r\n   ;branch=z9hG4bKnashds8;received=192.0.2.3' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com"' + \
          '\r\n   ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com' + \
          '\r\n   ;branch=z9hG4bK776asdhds ;received=192.0.2.1' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>;tag=a6c85cf' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:bob@192.0.2.4>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    p = Parser.new_dgram(msg)
    parsed_msg, data = Parser.parse(p)
    assert parsed_msg.body == 'Test'


def test_truncated_message():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\n'
    p = Parser.new_dgram(msg)
    with pytest.raises(TransportParserError) as excinfo:
        Parser.parse(p)
    assert 'Truncated' in str(excinfo.value)


def test_message_too_long_on_success_parse():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    p = Parser.new({'max_message_len': len(msg) - 1})
    p.add(msg)
    with pytest.raises(TransportParserError) as excinfo:
        Parser.parse(p)
    assert 'too long' in str(excinfo.value)


def test_not_too_long():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    p = Parser.new({'max_message_len': len(msg)})
    p.add(msg)
    Parser.parse(p)


def test_too_long_on_header_parse():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4'
    p = Parser.new({'max_message_len': len(msg) - 1})
    p.add(msg)
    with pytest.raises(TransportParserError) as excinfo:
        Parser.parse(p)
    assert 'too long' in str(excinfo.value)


def test_too_long_on_first_line():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0'
    p = Parser.new({'max_message_len': len(msg) - 1})
    p.add(msg)
    with pytest.raises(TransportParserError) as excinfo:
        Parser.parse(p)
    assert 'too long' in str(excinfo.value)


def test_invalid_method():
    msg = 'INV@TE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    p = Parser.new_dgram(msg)
    with pytest.raises(TransportParserError):
        Parser.parse(p)


def test_no_content_len():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\n\r\nTest'
    p = Parser.new_dgram(msg)
    pm, d = Parser.parse(p)
    assert pm.body == 'Test'


def test_invalid_content_len_dgram():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: invalid' + \
          '\r\n\r\nTest'
    p = Parser.new_dgram(msg)
    pm, d = Parser.parse(p)
    assert pm.body == 'Test'


def test_invalid_content_len_stream():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: invalid' + \
          '\r\n\r\nTest'
    p = Parser.new()
    p.add(msg)
    with pytest.raises(TransportParserError) as excinfo:
        Parser.parse(p)
    assert 'invalid content length' in str(excinfo.value)


def test_double_content_length():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\nContent-Length: 5' + \
          '\r\n\r\nTest'
    p = Parser.new()
    p.add(msg)
    with pytest.raises(TransportParserError):
        Parser.parse(p)


def test_incremental_data():
    msg_list = [
        #msg1
        'INVITE sip:bob@biloxi.com',
        # msg2
        ' SIP/2.0\r\n',
        #msg3
        'Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' +
        '\r\nMax-Forwards: 70' +
        '\r\nTo: Bob <sip:bob@biloxi.com>' +
        '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' +
        '\r\nCall-ID: a84b4c76e66710@pc33',
        #msg4
        '.atlanta.com' +
        '\r\nCSeq: 314159 INVITE' +
        '\r\nContact: <sip:alice@pc33.atlanta.com>' +
        '\r\nContent-Type: application/sdp' +
        '\r\nContent-Length: 1' +
        '0\r\n\r\n01234'
    ]
    p = Parser.new()
    for msg in msg_list:
        p.add(msg)
        more_data, data = Parser.parse(p)
        assert isinstance(more_data, MoreDataRequired)
    p.add('56789')
    p_msg, d = Parser.parse(p)
    assert p_msg.body == '0123456789'


@pytest.mark.parametrize('err_msg, msg', [('Bad status code', 'SIP/2.0 700 OK\r\n'),
                                          ('Bad status line', 'SIP/2.0 2000 OK\r\n'),
                                          ('Bad status line', 'SIP/3.0 200 OK\r\n'),
                                          ('Bad request line', 'Register\r\n'),
                                          ('bad header', 'REGISTER alice@example.com SIP/2.0' +
                                          '\r\nNo real header here' +
                                          '\r\nVia: X\r\n'),
                                          ('bad header',
                                           'REGISTER alice@example.com SIP/2.0\r\nNo real header here\r\n\r\n'),
                                          ('no headers', 'REGISTER alice@example.com SIP/2.0\r\n\r\n')])
def test_negative_cases(err_msg, msg):
    p = Parser.new()
    p.add(msg)
    with pytest.raises(TransportParserError) as excinfo:
        Parser.parse(p)
    assert err_msg in str(excinfo.value)
