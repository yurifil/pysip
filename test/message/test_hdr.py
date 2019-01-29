from pysip.message.hdr import Header
from pysip.message.hnames import HeaderKey
import pytest
from pysip.binary import to_bytes


@pytest.mark.parametrize('header_name, header_val, expected', [(b'Content-Length', b'10', 10),
                                                               (b'www-authenticate', b'10', 10)])
def test_as_integer(header_name, header_val, expected):
    header = Header(header_name)
    header.add_value(header_val)
    assert expected == header.as_integer()


def test_make_key():
    header_name = b'Content-Length'
    hdr = Header(header_name)
    assert hdr.key == HeaderKey(b'l')


def test_serialize_single_value():
    hdr = Header(b'Content-Length')
    hdr.add_value(b'10')
    assert b'Content-Length: 10' == hdr.serialize_to_bytes()


def test_serialize_multi_values():
    hdr = Header(b'Via')
    hdr.add_value(b'SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1')
    hdr.add_value(b'SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds')
    assert b'Via: SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' == hdr.serialize_to_bytes()


def test_comma_split_values():
    h1 = Header(b'route')
    h1.add_values([b"<sip:alice@atlanta.com>",
                   b"<sip:bob@biloxi.com>",
                   b"<sip:carol@chicago.com>"])
    h2 = Header(b'route')
    h2.add_values([b"<sip:alice@atlanta.com>, <sip:bob@biloxi.com>",
                   b"<sip:carol@chicago.com>"])
    h3 = Header(b'route')
    h3.add_values([b'<sip:alice@atlanta.com>, <sip:bob@biloxi.com>, <sip:carol@chicago.com>'])
    assert h1 == h2
    assert h1 == h3


def test_allow_compact():
    h = Header(b'Allow')
    h.add_values([b'ACK', b'INVITE', b'OPTIONS'])
    assert b'Allow: ACK, INVITE, OPTIONS' == h.serialize_to_bytes()


def test_contact():
    h = Header(b'Contact')
    h.add_values([b"sip:a@b", b"sip:a@c"])
    assert b'Contact: sip:a@b\r\nContact: sip:a@c' == h.serialize_to_bytes()


def test_allow_empty():
    h = Header(b'Allow')
    h.add_values([])
    assert b'A: B' == h.serialize_to_bytes(append_value=b'A: B')


def test_no_split_authenticate():
    h = Header(b'WWW-Authenticate')
    h.add_value(b'"Digest realm="atlanta.com", domain="sip:boxesbybob.com", qop="auth", '
                b'nonce="f84f1cec41e6cbe5aea9c8e88d359", opaque="", stale=FALSE, algorithm=MD5"')
    assert b'"Digest realm="atlanta.com", domain="sip:boxesbybob.com", qop="auth", ' \
           b'nonce="f84f1cec41e6cbe5aea9c8e88d359", opaque="", stale=FALSE, algorithm=MD5"' == to_bytes(h.values[0])


def test_no_split_authorization():
    h = Header(b'Authorization')
    h.add_value(b'Digest username="Alice", realm="atlanta.com",'
                b' nonce="84a4cc6f3082121f32b42a2187831a9e",'
                b' response="7587245234b3434cc3412213e5f113a5432"')
    assert b'Digest username="Alice", realm="atlanta.com",' \
           b' nonce="84a4cc6f3082121f32b42a2187831a9e",' \
           b' response="7587245234b3434cc3412213e5f113a5432"' == to_bytes(h.values[0])


def test_no_split_proxy_authenticate():
    h = Header(b'Proxy-Authenticate')
    h.add_value(b'Digest realm="atlanta.com",'
                b' domain="sip:ss1.carrier.com", qop="auth",'
                b' nonce="f84f1cec41e6cbe5aea9c8e88d359",'
                b' opaque="", stale=FALSE, algorithm=MD5')
    assert b'Digest realm="atlanta.com",' \
           b' domain="sip:ss1.carrier.com", qop="auth",' \
           b' nonce="f84f1cec41e6cbe5aea9c8e88d359",' \
           b' opaque="", stale=FALSE, algorithm=MD5' == to_bytes(h.values[0])


def test_name():
    assert 'Allow' == Header(b'Allow').name

'''
add_topmost_to_singleton_test() ->
    H0 = ersip_hdr:new(<<"To">>),
    ?assertError({api_error, _}, ersip_hdr:add_topmost([],H0)),
    ok.

'''