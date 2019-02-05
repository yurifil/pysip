from pysip.message.hdr_date import DateHeaderError, DateHeader, GMT
from pysip.message.hdr import Header
import pytest
import datetime


@pytest.mark.parametrize('dt_str', [
                                    # weekdays:
                                    'Sun, 1 Jul 2018 05:00:00 GMT',
                                    'Mon, 2 Jul 2018 05:00:00 GMT',
                                    'Tue, 3 Jul 2018 05:00:00 GMT',
                                    'Wed, 4 Jul 2018 05:00:00 GMT',
                                    'Thu, 5 Jul 2018 05:00:00 GMT',
                                    'Fri, 6 Jul 2018 05:00:00 GMT',
                                    'Sat, 7 Jul 2018 05:00:00 GMT',
                                    # months:
                                    'Mon, 1 Jan 2018 05:00:00 GMT',
                                    'Thu, 1 Feb 2018 05:00:00 GMT',
                                    'THu, 1 Mar 2018 05:00:00 GMT',
                                    'Sun, 1 Apr 2018 05:00:00 GMT',
                                    'Tue, 1 May 2018 05:00:00 GMT',
                                    'Fri, 1 Jun 2018 05:00:00 GMT',
                                    'Sun, 1 Jul 2018 05:00:00 GMT',
                                    'Wed, 1 Aug 2018 05:00:00 GMT',
                                    'Sat, 1 Sep 2018 05:00:00 GMT',
                                    'Mon, 1 Oct 2018 05:00:00 GMT',
                                    'Thu, 1 Nov 2018 05:00:00 GMT',
                                    'Sat, 1 Dec 2018 05:00:00 GMT'
                                    ])
def test_parse(dt_str):
    DateHeader(dt_str)


def test_check_date():
    dt = DateHeader("Fri, 16 Aug 1963 05:00:00 GMT")
    assert dt.date == (1963, 8, 16)
    assert dt.time == (5, 0, 0)


# TODO:
#  'Sun, 16 Aug 1963 05:00:00 GMT',
#  should raise exception
@pytest.mark.parametrize('dt_str', ['Fri, 31 Feb 2018 05:00:00 GMT',
                                    'Fra, 16 Aug 1963 05:00:00 GMT',
                                    'Fri, 16 Awg 1963 05:00:00 GMT',
                                    'Fri, 16 Aug some 05:00:00 GMT',
                                    'Fri, 16 Aug 1963 105:00:00 GM',
                                    'Fri, 16 Aug 1963 05:100:00 GMT',
                                    'Fri, 16 Aug 1963 05:00:100 GMT',
                                    'Fri  16 Aug 1963 05:00:00 GMT',
                                    'Fri, 16 Aug 1963 05:00:00',
                                    'Fri, 16 Aug 1963 05:00:00 +0004',
                                    'Fri, 16 Aug 1963 05:00:00 UTC',
                                    'Fri, 1 Feb 2018 05:00:00 UT',
                                    'Fri, 1 Feb 2018 05:00:00 EST',
                                    'Fri, 1 Feb 2018 05:00:00 EDT',
                                    'Fri, 1 Feb 2018 05:00:00 MST',
                                    'Fri, 1 Feb 2018 05:00:00 MDT',
                                    'Fri, 1 Feb 2018 05:00:00 PST',
                                    'Fri, 1 Feb 2018 05:00:00 PDT',
                                    Header('Date')
                                    ])
def test_parse_error(dt_str):
    with pytest.raises(DateHeaderError):
        DateHeader(dt_str)


def test_dt_initialize():
    DateHeader(datetime.datetime(2015, 8, 8, 11, 11, 11))
    DateHeader(datetime.datetime(2015, 8, 8, 11, 11, 11, tzinfo=GMT()))


def test_now():
    assert isinstance(DateHeader.now(), DateHeader)


@pytest.mark.parametrize('dt_str', ['Fri, 16 Aug 1963 05:00:00 GMT',
                                    'Tue, 31 Jul 2018 01:49:00 GMT'
                                    ])
def test_reassemble(dt_str):
    dt = DateHeader(dt_str)
    assert dt.assemble() == dt_str


def test_build():
    dth = Header('Date')
    dth.add_value('Tue, 31 Jul 2018 01:49:00 GMT')
    dth_values = ''.join(dth.values)
    dt = DateHeader(dth)
    assert dth_values == ''.join(dt.build().values)
