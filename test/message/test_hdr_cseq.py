from pysip.message.hdr_cseq import CSeqHeader, CSeqHeaderError
from pysip.message.hdr import Header, HeaderError
from pysip.message.method_unicode import Method, MethodError, INVITE, REGISTER
from pysip import PySIPException
import pytest


def test_initialize():
    cseq = CSeqHeader(method=Method('MYMETHOD'), number='23')
    assert cseq.method == Method('MYMETHOD') and cseq.number == 23


def test_parse_invite():
    hdr = Header('CSeq')
    hdr.add_value('314159 INVITE')
    cseq = CSeqHeader(header=hdr)
    assert cseq.number == 314159
    assert cseq.method == INVITE


def test_parse_register():
    hdr = Header('CSeq')
    hdr.add_value('1 REGISTER')
    cseq = CSeqHeader(header=hdr)
    assert cseq.number == 1
    assert cseq.method == REGISTER


@pytest.mark.parametrize('value', ['Not Valid',
                                   "314159 INVITE x",
                                   "314159 INVITE, 31 ACK",
                                   "123 INV@TE",
                                   "abc INVITE"])
def test_parse_error(value):
    hdr = Header('CSeq')
    hdr.add_value(value)
    with pytest.raises(PySIPException):
        CSeqHeader(header=hdr)


def test_no_value():
    hdr = Header('CSeq')
    with pytest.raises(CSeqHeaderError):
        CSeqHeader(header=hdr)


@pytest.mark.parametrize('value', ["314159 INVITE",
                                   "1 REGISTER",
                                   "99 MYMETHOD"])
def test_reassemble(value):
    cseq = CSeqHeader(header=value)
    assert cseq.assemble() == value


def test_build():
    h = Header("CSeq")
    h.add_value('314159 INVITE')
    hvalues = ''.join(h.values)
    cseq = CSeqHeader(header=h)
    cseq_values = ''.join(cseq.build().values)
    assert hvalues == cseq_values
