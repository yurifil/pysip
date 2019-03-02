from pysip.message.hdr_route import RouteHeader, RouteHeaderError, Route
from pysip.message.hdr import Header
from pysip.uri.uri import Uri
import pytest


def test_topmost_route():
    hdr = Header('Route')
    hdr.add_value('<sip:alice@atlanta.com>,<sip:carol@chicago.com>,<sip:bob@biloxi.com>')
    route_hdr = RouteHeader(hdr)
    uri = Uri('sip:alice@atlanta.com')
    assert route_hdr.first.uri == uri


def test_topmost_route_with_ext():
    hdr_with_ext = Header('Route')
    hdr_with_ext.add_value('<sip:alice@atlanta.com>;extension=1')
    route_hdr_with_ext = RouteHeader(hdr_with_ext)
    assert [('extension', '1')] == route_hdr_with_ext.first.params


def test_topmost_route_with_lr():
    hdr = Header('Route')
    hdr.add_value('<sip:alice@atlanta.com;lr>')
    route_hdr = RouteHeader(hdr)
    assert route_hdr.first.uri.params == {'lr': True}


def test_parse_route_error():
    with pytest.raises(RouteHeaderError):
        RouteHeader.parse_route('?')


def test_bad_middle():
    hdr = Header('Route')
    hdr.add_value('<sip:alice@atlanta.com>,?,<sip:bob@biloxi.com>')
    with pytest.raises(RouteHeaderError):
        RouteHeader(hdr)


def test_invalid_lr():
    hdr = Header('Route')
    hdr.add_value('<sip:alice@atlanta.com>;lr')
    route_hdr = RouteHeader(hdr)
    assert not route_hdr.first.uri.params


@pytest.mark.parametrize('val', ['?',
                                 '<sip:alice@atlanta.com>;x&',
                                 '<sip:alice@atlanta.com>;x&=x'
                                 ])
def test_parse_error(val):
    hdr = Header('Route')
    hdr.add_value(val)
    with pytest.raises(RouteHeaderError):
        RouteHeader(hdr)


def test_empty_route_set():
    hdr = Header('Route')
    route_hdr = RouteHeader(hdr)
    assert route_hdr.route_set.is_empty()


@pytest.mark.parametrize('val', ['<sip:alice@atlanta.com>,<sip:carol@chicago.com>,<sip:bob@biloxi.com>',
                                 '<sip:alice@atlanta.com>;test=1,<sip:carol@chicago.com>,<sip:bob@biloxi.com>',
                                 '<sip:alice@atlanta.com>;lr'
                                 ])
def test_eq(val):
    hdr1, hdr2 = Header('Route'), Header('Route'),
    hdr1.add_value(val)
    hdr2.add_value(val)
    assert hdr1 == hdr2
