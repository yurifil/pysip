from pysip.message.hdr_contact import ContactHeader, ContactHeaderError
from pysip.uri.uri import Uri
from pysip.message.qvalue import QValueError, QValue
from pysip import PySIPException
import pytest


@pytest.mark.parametrize('value', ['sip:a@b',
                                   'sip:a@b;q=1',
                                   '<sip:a@b>',
                                   '<sip:a@b>;q=1',
                                   '<sip:a@b>;expires=0',
                                   '<sip:a@b>;q=1;expires=0',
                                   '<sip:a@b>;Q=1;ExPiRes=0;a=b',
                                   '<sip:a@b>;a=b',
                                   '<sip:a@b>;a',
                                   'A B <sip:a@b>;a'])
def test_rebuild(value):
    contact = ContactHeader(value)
    rebuilt_contact = ContactHeader(contact.assemble())
    assert contact == rebuilt_contact


@pytest.mark.parametrize('value', ['a@b',
                                   'sip:a@b;q=a',
                                   'sip:a@b;q=2',
                                   '<sip:a@b>;expires=a',
                                   'A <sip:a@b>;q=a',
                                   'B <sip:a@b>;q=2',
                                   'C <sip:a@b>;expires=a',
                                   'C <sip:a@b>;?=$',
                                   'C <sip:a@b>;?',
                                   '<sip:a@b',
                                   'C <sip:a@b>;;;;;',
                                   'sip:a@b;v=a, some bad rest'])
def test_parse_error(value):
    with pytest.raises(ContactHeaderError):
        ContactHeader(value)


def test_expires():
    alice = ContactHeader('Alice <sip:alice@atlanta.com>;expires=20')
    assert alice.get_expires() == 20
    alice.set_expires(30)
    assert alice.get_expires() == 30
    assert "Alice <sip:alice@atlanta.com>;expires=30" == alice.assemble()
    alice_no_expires = ContactHeader("Alice <sip:alice@atlanta.com>")
    assert alice_no_expires.get_expires() is None
    assert alice_no_expires.get_expires(21) == 21
    alice45 = ContactHeader("Alice <sip:alice@atlanta.com>;Expires=45")
    assert alice45.get_expires() == 45


def test_qvalue():
    alice_no_qvalue = ContactHeader("Alice <sip:alice@atlanta.com>")
    alice1 = ContactHeader("Alice <sip:alice@atlanta.com>;q=1.0")
    assert QValue('1.0') == alice1.get_qvalue()
    alice1.set_qvalue(QValue('0.1'))
    assert QValue('0.1') == alice1.get_qvalue()
    assert QValue('0.13') == alice_no_qvalue.get_qvalue(default=QValue('0.13'))


def test_uri():
    alice = ContactHeader('Alice <sip:alice@atlanta.com>')
    assert Uri('sip:alice@atlanta.com') == alice.uri


def test_set_param():
    alice = ContactHeader("Alice <sip:alice@atlanta.com>")
    alice.set_param('expires', '30')
    assert alice.get_expires(None) == 30
    alice.set_param('q', '0.1')
    assert alice.get_qvalue(None) == QValue('0.1')
    assert alice.get_expires(None) == 30
    alice.set_param('myparam', 'Value')
    assert alice == ContactHeader('Alice <sip:alice@atlanta.com>;expires=30;q=0.1;myparam=Value')
    assert alice.get_param('MyParam') == 'Value'


@pytest.mark.parametrize('name, value', [("expires", "@"),
                                         ("q", "2"),
                                         ("@", "Value"),
                                         ("@", None)])
def test_set_param_error(name, value):
    alice = ContactHeader("Alice <sip:alice@atlanta.com>")
    with pytest.raises(ContactHeaderError):
        alice.set_param(name, value)
