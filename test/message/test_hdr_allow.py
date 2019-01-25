from pysip.message.hdr import Header
from pysip.message.hdr_allow import parse_header, AllowError
import pytest


@pytest.mark.parametrize('values', [b'INVITE, ACK, OPTIONS',
                                    b'INVITE,ACK,OPTIONS',
                                    b'INVITE'])
def test_parse_success(values):
    h = Header(b'Allow')
    h.add_value(values)
    parse_header(h)


@pytest.mark.parametrize('value', [b'', b'&'])
def test_parse_fail(value):
    with pytest.raises(AllowError):
        h = Header(b'Allow')
        h.add_value(value)
        parse_header(h)
