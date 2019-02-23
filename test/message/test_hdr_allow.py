from pysip.message.hdr import Header
from pysip.message.hdr_allow import AllowHeader, AllowError
import pytest


@pytest.mark.parametrize('values', [b'INVITE, ACK, OPTIONS',
                                    b'INVITE,ACK,OPTIONS',
                                    b'INVITE'])
def test_parse_success(values):
    h = Header('Allow')
    h.add_value(values)
    ah = AllowHeader()
    ah.parse(h)


@pytest.mark.parametrize('value', [b'', b'&'])
def test_parse_fail(value):
    with pytest.raises(AllowError):
        h = Header('Allow')
        h.add_value(value)
        ah = AllowHeader()
        ah.parse(h)
