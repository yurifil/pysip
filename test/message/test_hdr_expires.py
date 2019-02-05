from pysip.message.hdr_expires import ExpiresHeader, ExpiresHeaderError
from pysip.message.hdr import Header
import pytest


@pytest.mark.parametrize('expires', ['4294967296',
                                     'a',
                                     '0xff',
                                     '-1',
                                     '-100',
                                     '1.1',
                                     '5, 6',
                                     '',
                                     Header('Expires')
                                     ])
def test_parse_error(expires):
    with pytest.raises(ExpiresHeaderError):
        ExpiresHeader(expires)


@pytest.mark.parametrize('expires', ['0', '5', '4294967295'])
def test_rebuild(expires):
    exp_hdr = ExpiresHeader(expires)
    assert int(expires) == exp_hdr.expires
