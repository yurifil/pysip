from pysip.message.hdr_content_type import ContentTypeHeader, ContentTypeHeaderError, MIMEType
from pysip.message.hdr import Header
from pysip.message.parser_aux import ParserAUXError
from pysip import PySIPException
import pytest


def test_parse_no_params():
    header = Header('Content-Type')
    header.add_value('application/sdp')
    ct_hdr = ContentTypeHeader(header=header)
    assert ct_hdr.mime_type == MIMEType('application', 'sdp')


def test_parse_params():
    header = Header('Content-Type')
    header.add_value('text/html; charset=ISO-8859-4')
    ct_hdr = ContentTypeHeader(header=header)
    assert ct_hdr.mime_type == MIMEType('text', 'html')
    assert ct_hdr.params == [('charset', 'ISO-8859-4')]


def test_parse_quoted_params():
    header = Header('Content-Type')
    header.add_value('text/html; charset="ISO-8859-4"')
    ct_hdr = ContentTypeHeader(header=header)
    assert ct_hdr.mime_type == MIMEType('text', 'html')
    assert ct_hdr.params == [('charset', '"ISO-8859-4"')]


def test_parse_quoted_params2():
    header = Header('Content-Type')
    header.add_value('text / html; charset="ISO-8859-4"')
    ct_hdr = ContentTypeHeader(header=header)
    assert ct_hdr.mime_type == MIMEType('text', 'html')
    assert ct_hdr.params == [('charset', '"ISO-8859-4"')]


def test_parse_empty_header():
    header = Header('Content-Type')
    with pytest.raises(ContentTypeHeaderError):
        ContentTypeHeader(header)


def test_parse_multi():
    header = Header('Content-Type')
    header.add_value("application/sdp")
    header.add_value("text/html")
    with pytest.raises(ContentTypeHeaderError):
        ContentTypeHeader(header)


@pytest.mark.parametrize('header', ['text', 'text/html;charset=,'])
def test_parse_incomplete_string(header):
    with pytest.raises(PySIPException):
        ContentTypeHeader(header)


def test_parse_invalid_header():
    header = Header('Content-Type')
    header.add_value("text/html; charset=&")
    with pytest.raises(ParserAUXError):
        ContentTypeHeader(header)


@pytest.mark.parametrize('header', ["application/sdp", 'text/html;charset="ISO-8859-4"'])
def test_reassemble(header):
    ct_hdr = Header("Content-Type")
    ct_hdr.add_value(header)
    ct = ContentTypeHeader(ct_hdr)
    assert ct.assemble() == header


def test_build():
    header = Header('Content-Type')
    header.add_value("application/sdp")
    ct_hdr = ContentTypeHeader(header=header)
    hvalues = ''.join(header.values)
    ct_hvalues = ''.join(ct_hdr.build().values)
    assert hvalues == ct_hvalues
