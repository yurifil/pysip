from pysip.message.hdr import Header
from pysip.uri import PARAM_RECEIVED, PARAM_RPORT
from pysip.uri.transport import Transport
from pysip.message.hdr_via import ViaHeader, ViaHeaderError, SentBy, SentProtocol
import ipaddress
from pysip.message.branch import Branch
import pytest


def test_topmost_via():
    hdr = Header('Via')
    hdr.add_values(["SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1",
                    "SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"])
    via = ViaHeader.topmost_via(hdr)
    assert SentProtocol('SIP', '2.0', Transport('UDP')) == via.sent_protocol
    assert 'bigbox3.site3.atlanta.com' == via.sent_by.host
    assert 5060 == via.sent_by.port


def test_topmost_via_ipport():
    hdr = Header('via')
    hdr.add_values(["SIP/2.0/TCP 192.168.1.1:5090;branch=z9hG4bK77ef4c2312983.1",
                    "SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1",
                    "SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"])
    via = ViaHeader.topmost_via(hdr)
    assert SentProtocol('SIP', '2.0', Transport('TCP')) == via.sent_protocol
    assert ipaddress.IPv4Address('192.168.1.1') == via.sent_by.host
    assert 5090 == via.sent_by.port


def test_via_params():
    via = ViaHeader('SIP/2.0/TCP 192.168.1.1:5090;branch=z9hG4bK77ef4c2312983.1;rport;x=1;some')
    assert Branch('z9hG4bK77ef4c2312983.1') == via.branch
    assert via.rport is True
    assert via.get_raw_param('x') == '1'
    assert via.get_raw_param('rport') is None
    assert via.get_raw_param('some') is None


def test_via_params_2():
    hdr = Header('Via')
    hdr.add_value("SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=1.1.1.1;maddr=x.com")
    via = ViaHeader.topmost_via(hdr)
    assert via.branch == Branch('branch_v')
    assert via.ttl == 200
    assert via.received == ipaddress.IPv4Address('1.1.1.1')
    assert via.maddr == 'x.com'


def test_via_params_ipv6():
    hdr = Header('Via')
    hdr.add_value("SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=[::1];maddr=[::1]")
    via = ViaHeader.topmost_via(hdr)
    assert via.received == ipaddress.IPv6Address('::1')
    assert via.maddr == ipaddress.IPv6Address('::1')


def test_via_gen_params():
    hdr = Header('Via')
    hdr.add_value('SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;my_param=abc')
    via = ViaHeader.topmost_via(hdr)
    assert via.get_raw_param('my_param') == 'abc'


def test_topmost_via_with_spaces():
    hdr = Header('Via')
    hdr.add_value('SIP / 2.0 / UDP first.example.com: 4000;ttl=16 ;maddr=224.2.0.1 ;branch=z9hG4bKa7c6a8dlze.1')
    via = ViaHeader.topmost_via(hdr)
    assert via.sent_protocol.name == 'SIP'
    assert via.sent_protocol.version == '2.0'
    assert via.sent_protocol.transport == Transport('UDP')
    assert via.branch == Branch('z9hG4bKa7c6a8dlze.1')
    assert via.maddr == ipaddress.IPv4Address('224.2.0.1')
    assert via.ttl == 16
    assert via.sent_by.host == 'first.example.com'
    assert via.sent_by.port == 4000


@pytest.mark.parametrize('via_str', ['SIP+2.0/UDP bigbox3.site3.atlanta.com;branch=z',
                                     '',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com::',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com::5060',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com:0',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com:65536',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com:-1',
                                     'SIP/2.0/UDP [:5060',
                                     'SIP/2.0/UDP []:5060',
                                     'SIP/2.0/UDP []',
                                     'SIP/2.0/UDP [',
                                     'SIP/2.0/UDP -1.-1.-1.-1:5060',
                                     'SIP/2.0+UDP 1.1.1.1:5060',
                                     'SIP/2.0/$   1.1.1.1:5060',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;ttl=256',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;ttl=-1',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;ttl=a',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;received=a.b.c.d',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;maddr=?',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;branch="xyz"',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;my_param="x',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=0',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=-0',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=65536',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=A'
                                     ])
def test_parse_fail(via_str):
    with pytest.raises(ViaHeaderError):
        ViaHeader(via_str)


def test_via_branch():
    branch_value = 'z9hG4bK776asdhds'
    branch = Branch(branch_value)
    via = ViaHeader('SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=' + branch_value)
    assert via.branch == branch
    assert via.branch.make_key() == branch.make_key()


@pytest.mark.parametrize('via_str1, via_str2', [('SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1',
                                                 'SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1'),
                                                ('SIP/2.0/UDP BIGBOX3.SITE3.ATLANTA.COM;BRANCH=Z9HG4BK77EF4C2312983.1',
                                                 'SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1'),
                                                ('SIP/2.0/UDP bigbox3.site3.atlanta.com;ttl=1',
                                                 'SIP/2.0/UDP bigbox3.site3.atlanta.com;TTL=1'),
                                                ('SIP/2.0/UDP bigbox3.site3.atlanta.com;maddr=x.com',
                                                 'SIP/2.0/UDP bigbox3.site3.atlanta.com;maddr=X.COM'),
                                                ('SIP/2.0/UDP bigbox3.site3.atlanta.com;received=1.1.1.1',
                                                 'SIP/2.0/UDP bigbox3.site3.atlanta.com;received=1.1.1.1'),
                                                ('SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=10',
                                                 'SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=10'),
                                                ('SIP/2.0/UDP bigbox3.site3.atlanta.com;rport',
                                                 'SIP/2.0/UDP bigbox3.site3.atlanta.com;rport'),
                                                ('SIP/2.0/UDP bigbox3.site3.atlanta.com;some=1',
                                                 'SIP/2.0/UDP bigbox3.site3.atlanta.com;SOMe=1'),
                                                ('SIP/2.0/UDP bigbox3.site3.atlanta.com;some',
                                                 'SIP/2.0/UDP bigbox3.site3.atlanta.com;SOMe'),
                                                ('SIP/2.0/UDP bigbox3.site3.atlanta.com',
                                                 'SIP/2.0/UDP bigbox3.site3.atlanta.com:5060'),
                                                # TODO: this host should be valid somehow:
                                                # ('SIP/2.0/UDP bigbox3.site3.atlanta.com.',
                                                # 'SIP/2.0/UDP bigbox3.site3.atlanta.com')
                                                ])
def test_eq(via_str1, via_str2):
    via1 = ViaHeader(via_str1)
    via2 = ViaHeader(via_str2)
    assert via1 == via2


@pytest.mark.parametrize('via_str1, via_str2', [('SIP/2.0/UDP bigbox3.site3.atlanta.com',
                                                 'SIP/2.0/TCP bigbox3.site3.atlanta.com'),
                                                ('SIP/2.0/UDP bigbox3.site3.atlanta.com',
                                                 'SIP/3.0/UDP bigbox3.site3.atlanta.com'),
                                                ('SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=2',
                                                 'SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=1'),
                                                ('SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=2',
                                                 'SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=1'),
                                                ('SIP/2.0/UDP bigbox3.site3.atlanta.com;rport',
                                                 'SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=1'),
                                                ('SIP/2.0/UDP bigbox3.site3.atlanta.com',
                                                 'SIP/2.0/UDP bigbox3.site3.atlanta.com;rport'),
                                                ('SIP/2.0/UDP bigbox3.site3.atlanta.com',
                                                 'SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=1')
                                                ])
def test_ne(via_str1, via_str2):
    via1 = ViaHeader(via_str1)
    via2 = ViaHeader(via_str2)
    assert via1 != via2


def test_sent_by_key():
    via1 = ViaHeader('SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=x')
    via2 = ViaHeader('SIP/2.0/UDP BIGBOX3.SITE3.ATLANTA.COM;branch=y')
    assert via1.sent_by == via2.sent_by


@pytest.mark.parametrize('via_str', ['SIP/2.0/UDP bigbox3.site3.atlanta.com:500;ttl=1',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1',
                                     'SIP/2.0/UDP BIGBOX3.SITE3.ATLANTA.COM;BRANCH=Z9HG4BK77EF4C2312983.1',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;ttl=1',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;maddr=x.com',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;received=1.1.1.1',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;rport',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=1234',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;some=1',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com;some',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com:5060',
                                     'SIP/2.0/UDP bigbox3.site3.atlanta.com'
                                     ])
def test_assemble(via_str):
    via1 = ViaHeader(via_str)
    hdr = Header('Via')
    hdr.add_value(via1.assemble())
    via2 = ViaHeader.topmost_via(hdr)
    assert via1 == via2


def test_set_param_received_ipv4():
    via = ViaHeader('SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=1.1.1.1;maddr=x.com')
    via.set_param(PARAM_RECEIVED, '2.2.2.2')
    assert via.received == ipaddress.IPv4Address('2.2.2.2')
    via = ViaHeader('SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=1.1.1.1;maddr=x.com')
    via.set_param(PARAM_RECEIVED, ipaddress.IPv4Address('2.2.2.2'))
    assert via.received == ipaddress.IPv4Address('2.2.2.2')


def test_set_param_received_ipv6():
    via = ViaHeader('SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=1.1.1.1;maddr=x.com')
    via.set_param(PARAM_RECEIVED, '[::1]')
    assert via.assemble() == 'SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=[::1];maddr=x.com'
    assert via.received == ipaddress.IPv6Address('::1')
    via = ViaHeader('SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=1.1.1.1;maddr=x.com')
    via.set_param(PARAM_RECEIVED, ipaddress.IPv6Address('::1'))
    assert via.assemble() == 'SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=[::1];maddr=x.com'
    assert via.received == ipaddress.IPv6Address('::1')


@pytest.mark.parametrize('rec_str', ['x', '.'])
def test_set_received_error(rec_str):
    via = ViaHeader('SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=1.1.1.1;maddr=x.com')
    with pytest.raises(ViaHeaderError):
        via.set_param(PARAM_RECEIVED, rec_str)


def test_set_rport():
    via = ViaHeader('SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;rport=1234;maddr=x.com')
    via.set_param(PARAM_RPORT, 4321)
    assert via.assemble() == 'SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;rport=4321;maddr=x.com'
    assert via.rport == 4321


def test_set_rport_to_true():
    via = ViaHeader('SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;rport=1234;maddr=x.com')
    via.set_param(PARAM_RPORT, True)
    assert via.assemble() == 'SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;rport;maddr=x.com'
    assert via.rport is True


@pytest.mark.parametrize('rport', [False,
                                   'aaaa',
                                   65536,
                                   -1,
                                   0])
def test_set_rport_error(rport):
    via = ViaHeader('SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;rport=4321;maddr=x.com')
    with pytest.raises(ViaHeaderError):
        via.set_param(PARAM_RPORT, rport)
