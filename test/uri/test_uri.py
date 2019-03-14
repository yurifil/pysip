from pysip.uri.uri import Uri
from pysip.uri import URIParseError, UserParseError, ParamParseError, PARAM_TRANSPORT, PARAM_LR, PARAM_USER,\
    PARAM_MADDR, PARAM_TTL, HostParseError, SIPUriError
from pysip import PySIPException
import ipaddress
import pytest


def test_uri_simple_parse():
    uri_str = b'sip:a@b:5090'
    uri = Uri(uri_str)
    assert ('a', 'b', 5090) == (uri.user, uri.host, uri.port)


def test_uri_ipv4_host_parse():
    uri_str = b'sip:a@1.2.3.4:5090'
    uri = Uri(uri_str)
    assert isinstance(uri.host, ipaddress.IPv4Address)
    assert ('a', ipaddress.IPv4Address('1.2.3.4'), 5090) == (uri.user, uri.host, uri.port)


def test_uri_ipv6_host_parse():
    uri_str = b'sip:a@[::1]:5090'
    uri = Uri(uri_str)
    assert isinstance(uri.host, ipaddress.IPv6Address)
    assert ('a', ipaddress.IPv6Address('::1'), 5090) == (uri.user, uri.host, uri.port)


def test_uri_sips_scheme_parse():
    uri_str = b'sips:a@b:5090'
    uri = Uri(uri_str)
    assert ('sips', 'a', 'b', 5090) == (uri.scheme, uri.user, uri.host, uri.port)


def test_uri_sips_with_passwd_parse():
    uri_str = b'sips:a:b@b:5090'
    uri = Uri(uri_str)
    assert ('sips', 'a:b', 'b', 5090) == (uri.scheme, uri.user, uri.host, uri.port)


def test_percent_sign_passwd():
    uri_str = b"sips:a:%20@b:5090"
    uri = Uri(uri_str)
    assert ('sips', 'a:%20', 'b', 5090) == (uri.scheme, uri.user, uri.host, uri.port)


def test_percent_sign_user():
    uri_str = b"sips:%20@b:5090"
    uri = Uri(uri_str)
    assert ('sips', '%20', 'b', 5090) == (uri.scheme, uri.user, uri.host, uri.port)


def test_invalid_scheme():
    uri_str = b"?:a@b:5090"
    with pytest.raises(URIParseError):
        Uri(uri_str)


@pytest.mark.parametrize('uri_str', [b'sip:%@b:5090',
                                     b'sip:@b:5090',
                                     b'sip::a@b:5090',
                                     b'sip:a:%@b:5090'])
def test_invalid_username(uri_str):
    with pytest.raises(UserParseError):
        Uri(uri_str)


def test_empty_user():
    uri_str = b'sip:b:5090'
    uri = Uri(uri_str)
    assert (None, 'b', 5090) == (uri.user, uri.host, uri.port)


def test_host_only():
    uri_str = b'sip:b'
    uri = Uri(uri_str)
    assert 'b' == uri.host


@pytest.mark.parametrize('uri_str', [b'sip:%:5090',
                                     b'sip:%',
                                     b'sip:a.-',
                                     b'sip:b:x',
                                     b'sip:[::1',
                                     b'sip:[::1]:',
                                     b'sip:[::1]x'
                                     ])
def test_invalid_hostport(uri_str):
    with pytest.raises(PySIPException):
        Uri(uri_str)


def test_params_transport_tcp():
    uri_str = b'sip:b;transport=tcp'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_TRANSPORT: 'tcp'} == uri.params


def test_params_transport_udp():
    uri_str = b'sip:b;transport=udp'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_TRANSPORT: 'udp'} == uri.params


def test_params_transport_tls():
    uri_str = b'sip:b;transport=tls'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_TRANSPORT: 'tls'} == uri.params


def test_params_transport_ws():
    uri_str = b'sip:b;transport=ws'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_TRANSPORT: 'ws'} == uri.params


def test_params_transport_wss():
    uri_str = b'sip:b;transport=wss'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_TRANSPORT: 'wss'} == uri.params


def test_params_transport_wssnew():
    uri_str = b'sip:b;transport=wssnew'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_TRANSPORT: 'wssnew'} == uri.params


def test_params_invalid_transport():
    uri_str = b'sip:b;transport=&'
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_params_invalid_transport_second_param():
    uri_str = b'sip:b;transport=&;user=phone'
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_multi_params():
    uri_str = b'sip:b;transport=tcp;user=phone'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_TRANSPORT: 'tcp', PARAM_USER: 'phone'} == uri.params


def test_params_lr():
    uri_str = b'sip:b;lr'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_LR: True} == uri.params


def test_maddr():
    uri_str = b'sip:b;maddr=1.1.1.1'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_MADDR: ipaddress.IPv4Address('1.1.1.1')} == uri.params


def test_invalid_maddr():
    uri_str = b'sip:b;maddr=&'
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_user_param_phone():
    uri_str = b'sip:b;user=phone'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_USER: 'phone'} == uri.params


def test_user_param_ip():
    uri_str = b'sip:b;user=ip'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_USER: 'ip'} == uri.params


def test_user_param_something():
    uri_str = b'sip:b;user=something'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_USER: 'something'} == uri.params


def test_invalid_user_param():
    uri_str = b'sip:b;user=&'
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_empty_user_param():
    uri_str = b'sip:b;user='
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_ttl_param():
    uri_str = b'sip:b;ttl=1'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_TTL: 1} == uri.params


def test_ttl_param_literal():
    uri_str = b'sip:b;ttl=a'
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_ttl_param_negative():
    uri_str = b'sip:b;ttl=-1'
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_ttl_param_too_big():
    uri_str = b'sip:b;ttl=256'
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_some_param():
    uri_str = b'sip:b;Some=1'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {'some': '1'} == uri.params


def test_headers():
    uri_str = b'sip:b?Some=1&Another=2'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {'some': '1', 'another': '2'} == uri.headers


def test_invalid_uri():
    uri_str = b'a@b'
    with pytest.raises(URIParseError):
        Uri(uri_str)


def test_make_sips():
    expected_uri = Uri(b'sips:Alice@atlanta.com:8083')
    uri = Uri.make(scheme=b'sips', user=b'Alice', host=b'atlanta.com', port=8083)
    assert expected_uri == uri


def test_make_sip():
    expected_uri2 = Uri(b'sip:Alice@atlanta.com:5061')
    uri2 = Uri.make(scheme=b'sip', user=b'Alice', host=b'atlanta.com', port=b'5061')
    assert expected_uri2 == uri2


def test_make_invalid_hostname():
    with pytest.raises(HostParseError):
        Uri.make(host=b'-ab')


def test_make_invalid_part():
    with pytest.raises(SIPUriError):
        Uri.make(x=b'a-b')


def test_make_invalid():
    with pytest.raises(Exception):
        Uri.make(b'x')


def test_compare_eq():
    assert Uri(b'sip:%61lice@atlanta.com;transport=TCP') == Uri(b'sip:alice@AtLanTa.CoM;Transport=tcp')
    assert Uri(b'sip:carol@chicago.com') == Uri(b'sip:carol@chicago.com;newparam=5')
    assert Uri(b'sip:carol@chicago.com') == Uri(b'sip:carol@chicago.com;security=on')
    assert Uri(b'sip:carol@chicago.com;newparam=5') == Uri(b'sip:carol@chicago.com;security=on')
    assert Uri(b'sip:biloxi.com;transport=tcp;method=REGISTER?to=sip:bob%40biloxi.com') == \
           Uri(b'sip:biloxi.com;method=REGISTER;transport=tcp?to=sip:bob%40biloxi.com')
    assert Uri(b'sip:alice@atlanta.com?subject=project%20x&priority=urgent') == \
           Uri(b'sip:alice@atlanta.com?priority=urgent&subject=project%20x')
    assert Uri(b'sip:1.1.1.1;transport=tcp') == Uri(u'sip:1.1.1.1;transport=tcp')
    assert Uri(b'sip:[::1];transport=tcp') == Uri(u'sip:[::1];transport=tcp')
    assert Uri(b'sip:[::1]:5060;transport=tcp') == Uri(b'sip:[::1]:5060;transport=tcp')


def test_compare_ne():
    assert Uri(b'SIP:ALICE@AtLanTa.CoM;Transport=udp') != Uri(b'sip:alice@AtLanTa.CoM;Transport=UDP')
    assert Uri(b'sip:bob@biloxi.com') != Uri(b'sip:bob@biloxi.com:5060')
    assert Uri(b'sip:bob@biloxi.com') != Uri(b'sip:bob@biloxi.com;transport=udp')
    assert Uri(b'sip:bob@biloxi.com') != Uri(b'sip:bob@biloxi.com:6000;transport=tcp')
    assert Uri(b'sip:carol@chicago.com') != Uri(b'sip:carol@chicago.com?Subject=next%20meeting')


@pytest.mark.parametrize('uri_str', [b'sip:1.1.1.1;transport=tcp',
                                     b'sips:%20@b:5090',
                                     b'sip:b;transport=wss',
                                     b'sip:b;maddr=1.1.1.1',
                                     b'sip:b;ttl=1',
                                     b'sip:b;lr',
                                     b'sip:b;user=phone',
                                     b'sip:b;user=ip',
                                     b'sip:b;user=Some',
                                     b'sip:b;myparam=Param',
                                     b'sip:b;myparam',
                                     b'tel:b;myparam'
                                     ])
def test_assemble(uri_str):
    uri = Uri(uri_str)
    assert uri.uri == uri.parser_impl.from_inner(uri_str)


def test_get():
    uri = Uri(b'sip:bob@1.1.1.1:5091')
    assert 'sip' == uri.get(part='scheme')
    assert 'bob' == uri.get(part='user')
    assert ipaddress.IPv4Address('1.1.1.1') == uri.get(part='host')
    assert 5091 == uri.get(part='port')


def test_user_setter():
    uri = Uri(b'sip:bob@1.1.1.1:5091')
    uri.user = b'alice'
    assert uri.user == 'alice'
    assert uri.uri == 'sip:alice@1.1.1.1:5091'


def test_host_setter():
    uri = Uri(b'sip:bob@1.1.1.1:5091')
    uri.host = b'biloxi.com'
    assert uri.host == 'biloxi.com'
    assert uri.uri == 'sip:bob@biloxi.com:5091'


def test_port_setter():
    uri = Uri(b'sip:bob@1.1.1.1:5091')
    uri.port = 5092
    assert uri.port == 5092
    assert uri.uri == 'sip:bob@1.1.1.1:5092'


def test_port_setter_none_unicode():
    uri = Uri(b'sip:bob@1.1.1.1:5091')
    uri.port = None
    assert uri.port is None
    assert uri.uri == 'sip:bob@1.1.1.1'


def test_three_params():
    uri = Uri(b'sip:carol@chicago.com;param1=value1;param2=value2;param3=value3')
    assert {'param1': 'value1', 'param2': 'value2', 'param3': 'value3'} == uri.params
