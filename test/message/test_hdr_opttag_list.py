from pysip.message.hdr_opttag_list import OptTagListHeader, OptTagListHeaderError
from pysip.message.option_tag import OptionTag
from pysip.message.hdr import Header
import pytest


@pytest.mark.parametrize('opttaglist, expected', [('100rel, timer', {OptionTag('100rel'), OptionTag('timer')}),
                                                  ('100rel,timer', {OptionTag('100rel'), OptionTag('timer')}),
                                                  ('100rel', {OptionTag('100rel')})
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
