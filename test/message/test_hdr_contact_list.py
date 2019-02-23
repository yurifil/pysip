from pysip.message.hdr_contact_list import ContactHeaderList, ContactHeaderListError
from pysip.message.hdr import Header
import pytest


@pytest.mark.parametrize('val_list', [('*',),
                                      ('sip:a@b',),
                                      ('sip:a@b,sip:b@a',),
                                      ('sip:a@b, sip:b@a,sip:c@d', 'sip:d@v')])
def test_parse(val_list):
    hdr = Header('Contact')
    for val in val_list:
        hdr.add_value(val)
    ct_lst_hdr = ContactHeaderList(hdr)


@pytest.mark.parametrize('val_list', [('*,sip:a@b',),
                                      ('sip:a@b', '*',)
                                      ])
def test_parse_error(val_list):
    hdr = Header('Contact')
    for val in val_list:
        hdr.add_value(val)
    with pytest.raises(ContactHeaderListError):
        ContactHeaderList(hdr)

