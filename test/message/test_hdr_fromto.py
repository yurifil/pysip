from pysip.message.hdr_fromto import FromToError, FromToHeader
from pysip import PySIPException
from pysip.uri.uri import Uri
from pysip.message.hdr import Header
from pysip.message.hparams import HParams
import pytest


@pytest.mark.parametrize('display_name, tag, fromto', [('Alice', '1928301774', 'Alice <sip:alice@atlanta.com>;tag=1928301774'),
                                                       ('Alice', None, 'Alice <sip:alice@atlanta.com>'),
                                                       ('', '1928301774', '<sip:alice@atlanta.com>;tag=1928301774'),
                                                       ('', '100', '<sip:alice@atlanta.com>;tag=100')
                                                       ])
def test_parse_tag(display_name, tag, fromto):
    uri_str = 'sip:alice@atlanta.com'
    uri = Uri(uri_str)
    fromto_hdr = FromToHeader(fromto)
    assert fromto_hdr.display_name == display_name
    assert fromto_hdr.uri == uri
    assert fromto_hdr.tag == tag


def test_parse_param():
    uri_str = 'sip:alice@atlanta.com'
    uri = Uri(uri_str)
    fromto_hdr = FromToHeader('<sip:alice@atlanta.com>;Some=y')
    hp = HParams()
    hp.set_raw('Some', 'y')
    assert fromto_hdr.params == hp
    assert fromto_hdr.uri == uri


@pytest.mark.parametrize('fromto_str', ['a@b',
                                        'sip:a@b;tag="123"',
                                        'sip:a@b;my_param=&',
                                        'Bond, James <sip:bond@mi6.uk>',
                                        'sip:example.net;?'
                                        ])
def test_parse_error(fromto_str):
    with pytest.raises(PySIPException):
        FromToHeader(fromto_str)


def test_parse_empty_header():
    hdr = Header('To')
    with pytest.raises(FromToError):
        FromToHeader(hdr)


def test_parse_multi_values():
    hdr = Header('To')
    hdr.add_value("Alice <sip:a@b>")
    hdr.add_value("Bob <sip:b@a>")
    with pytest.raises(FromToError):
        FromToHeader(hdr)


@pytest.mark.parametrize('tag, fromto_str', [('1928301774', 'sip:a@b;tag=1928301774'),
                                             ('abc', 'sip:a@b;tag=ABC'),
                                             (None, 'sip:a@b')])
def test_tag(tag, fromto_str):
    fromto = FromToHeader(fromto_str)
    assert fromto.tag_key == tag


@pytest.mark.parametrize('ft_str', ['<sip:a@b>',
                                    'Alice <sip:a@b>',
                                    'Alice <sip:alice@atlanta.com>;tag=88sja8x',
                                    '"A. G. Bell" <sip:agb@bell-telephone.com>;tag=a48s',
                                    '"A. G. Bell" <sip:agb@bell-telephone.com>;tag=a48s;myparam=Value'
                                    ])
def test_reassemble(ft_str):
    ft_hdr = FromToHeader(ft_str)
    hdr = Header('To')
    hdr.add_value(ft_hdr.assemble())
    ft_hdr2 = FromToHeader(hdr)
    print(f'{ft_hdr} == {ft_hdr2}')
    assert ft_hdr == ft_hdr2


@pytest.mark.parametrize('ft_str, ft_str_exp', [('sip:example.net;a', '<sip:example.net>;a'),
                                                ('sip:example.net;tag=88sja8x', '<sip:example.net>;tag=88sja8x')])
def test_reassemble_without_quotes(ft_str, ft_str_exp):
    ft_hdr = FromToHeader(ft_str)
    assert ft_hdr.assemble() == ft_str_exp


def test_set_tag():
    ft_hdr = FromToHeader('Alice <sip:alice@atlanta.com>')
    ft_hdr.tag = '88sja8x'
    assert ft_hdr.assemble() == 'Alice <sip:alice@atlanta.com>;tag=88sja8x'


@pytest.mark.skip
def test_rfc4475_crazy_example():
    ft_hdr = FromToHeader(r'"BEL:\<hex>07</hex> NUL:\<hex>00</hex> DEL:\<hex>7F</hex>" <sip:1_unusual.URI~(to-be!sure)&isn\'t+it$/crazy?,/;;*@example.com>')
    assert ft_hdr.display_name == r'"BEL:\<hex>07</hex> NUL:\<hex>00</hex> DEL:\<hex>7F</hex>"'
