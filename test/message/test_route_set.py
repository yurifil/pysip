from pysip.message.route_set import RouteSetError, RouteSet
from pysip.message.hdr_route import RouteHeader
import pytest


def test_is_empty():
    rs = RouteSet()
    assert rs.is_empty()
    route = RouteHeader('<sip:a@b>')
    rs.add_first(route)
    assert not rs.is_empty()


def test_first_and_last():
    rs = RouteSet()
    with pytest.raises(RouteSetError):
        l = rs.last
    with pytest.raises(RouteSetError):
        f = rs.first
    r1 = RouteHeader('<sip:a@b>')
    rs.add_first(r1)
    assert r1 == rs.first
    assert r1 == rs.last
    r2 = RouteHeader('<sip:b@a>')
    rs.add_first(r2)
    assert r1 == rs.last
    assert r2 == rs.first
    rs.remove_first()
    assert r1 == rs.first
    assert r1 == rs.last
    rs.remove_first()
    with pytest.raises(RouteSetError):
        l = rs.last
    with pytest.raises(RouteSetError):
        f = rs.first





