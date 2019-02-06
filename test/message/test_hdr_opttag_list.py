from pysip.message.hdr_opttag_list import OptTagListHeader, OptTagListHeaderError
from pysip.message.hdr import Header
import pytest


@pytest.mark.parametrize('opttaglist, expected', [('100rel, timer', set(['100rel', 'timer'])),
                                                  ('100rel,timer', set(['100rel', 'timer'])),
                                                  ('100rel', set(['100rel']))
                                                  ])
def test_parse(opttaglist, expected):
    hdr = Header('Supported')
    hdr.add_value(opttaglist)
    otl_hdr = OptTagListHeader(hdr)
    assert otl_hdr.opt_tag_set == expected


@pytest.mark.parametrize('opttaglist', ['', '&'])
def test_parse_error(opttaglist):
    with pytest.raises(OptTagListHeaderError):
        OptTagListHeader(opttaglist)
