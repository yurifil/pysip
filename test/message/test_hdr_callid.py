import pytest
from pysip.message.hdr import Header
from pysip.binary import to_bytes
from pysip.message.hdr_callid import CallIDError, CallIDHeader, WORD_CHARS
import string


def make_alphanum_string():
    return string.ascii_lowercase + string.ascii_uppercase + string.digits


@pytest.mark.parametrize('value', ['a@b',
                                   to_bytes(WORD_CHARS + make_alphanum_string() + '@' + make_alphanum_string() +
                                            WORD_CHARS),
                                   to_bytes(WORD_CHARS + make_alphanum_string())])
def test_parse_header_success(value):
    h = Header('Call-ID')
    h.add_value(value)
    assert h == CallIDHeader(value).build('Call-ID')


@pytest.mark.parametrize('value', [',', None, '', 'a@@b', 'a@b, c@d'])
def test_parse_header_fail(value):
    with pytest.raises(CallIDError):
        CallIDHeader(value)
