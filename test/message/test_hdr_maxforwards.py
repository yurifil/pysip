from pysip.message.hdr_maxforwards import MaxForwardsHeader, MaxForwardsHeaderError
from pysip.message.hdr import Header
import pytest


@pytest.mark.parametrize('maxforwards, expected', [('70', 70),
                                                   ('0', 0),
                                                   ('200', 200)])
def test_parse(maxforwards, expected):
    mf_hdr = MaxForwardsHeader(maxforwards)
    hdr = Header('Max-Forwards')
    hdr.add_value(maxforwards)
    mf_hdr_2 = MaxForwardsHeader(hdr)
    assert mf_hdr == mf_hdr_2
    assert mf_hdr_2.maxforwards == mf_hdr.maxforwards == expected


@pytest.mark.parametrize('maxforwards', ['',
                                         'a@b',
                                         '70,1'])
def test_parse_error(maxforwards):
    with pytest.raises(MaxForwardsHeaderError):
        MaxForwardsHeader(maxforwards)


def test_decrement():
    mf_hdr = MaxForwardsHeader(1)
    mf_hdr.decrement()
    assert mf_hdr.maxforwards == 0


def test_decrement_error():
    mf_hdr = MaxForwardsHeader(0)
    with pytest.raises(MaxForwardsHeaderError):
        mf_hdr.decrement()
