from pysip.uri.uri import Uri, SIPUriError
from pysip.uri.uri_parser import URIParseError, UserParseError, PARAM_TRANSPORT, ParamParseError, PARAM_USER, PARAM_LR,\
    PARAM_TTL, HostParseError
import ipaddress
import pytest


def test_uri_simple_parse_unicode():
    uri_str = u'sip:a@b:5090'
    uri = Uri(uri_str)
    assert ('a', 'b', 5090) == (uri.user, uri.host, uri.port)


def test_uri_ipv4_host_parse_unicode():
    uri_str = u'sip:a@1.2.3.4:5090'
    uri = Uri(uri_str)
    assert isinstance(uri.host, ipaddress.IPv4Address)
    assert ('a', ipaddress.IPv4Address('1.2.3.4'), 5090) == (uri.user, uri.host, uri.port)


def test_uri_ipv6_host_parse_unicode():
    uri_str = u'sip:a@[::1]:5090'
    uri = Uri(uri_str)
    assert isinstance(uri.host, ipaddress.IPv6Address)
    assert ('a', ipaddress.IPv6Address('::1'), 5090) == (uri.user, uri.host, uri.port)


def test_uri_sips_scheme_parse_unicode():
    uri_str = u'sips:a@b:5090'
    uri = Uri(uri_str)
    assert ('sips', 'a', 'b', 5090) == (uri.scheme, uri.user, uri.host, uri.port)


def test_uri_sips_with_passwd_parse_unicode():
    uri_str = u'sips:a:b@b:5090'
    uri = Uri(uri_str)
    assert ('sips', 'a:b', 'b', 5090) == (uri.scheme, uri.user, uri.host, uri.port)


def test_percent_sign_user_unicode():
    uri_str = u"sips:%20@b:5090"
    uri = Uri(uri_str)
    assert ('sips', '%20', 'b', 5090) == (uri.scheme, uri.user, uri.host, uri.port)


def test_percent_sign_passwd_unicode():
    uri_str = u"sips:a:%20@b:5090"
    uri = Uri(uri_str)
    assert ('sips', 'a:%20', 'b', 5090) == (uri.scheme, uri.user, uri.host, uri.port)


def test_invalid_scheme_unicode():
    uri_str = u"?:a@b:5090"
    with pytest.raises(URIParseError):
        Uri(uri_str)


@pytest.mark.parametrize('uri_str', [u'sip:%@b:5090',
                                     u'sip:@b:5090',
                                     u'sip::a@b:5090',
                                     u'sip:a:%@b:5090'])
def test_invalid_username_unicode(uri_str):
    with pytest.raises(UserParseError):
        Uri(uri_str)


def test_empty_user_unicode():
    uri_str = u'sip:b:5090'
    uri = Uri(uri_str)
    assert (None, 'b', 5090) == (uri.user, uri.host, uri.port)


def test_host_only_unicode():
    uri_str = u'sip:b'
    uri = Uri(uri_str)
    assert 'b' == uri.host


@pytest.mark.parametrize('uri_str', [u'sip:%:5090',
                                     u'sip:%',
                                     u'sip:a.-',
                                     u'sip:b:x',
                                     u'sip:[::1',
                                     u'sip:[::1]:',
                                     u'sip:[::1]x'
                                     ])
def test_invalid_hostport_unicode(uri_str):
    with pytest.raises(ValueError):
        Uri(uri_str)


def test_params_transport_unicode():
    uri_str = u'sip:b;transport=tcp'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_TRANSPORT: 'tcp'} == uri.params


def test_params_transport_udp_unicode():
    uri_str = u'sip:b;transport=udp'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_TRANSPORT: 'udp'} == uri.params


def test_params_transport_tls_unicode():
    uri_str = u'sip:b;transport=tls'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_TRANSPORT: 'tls'} == uri.params


def test_params_transport_ws_unicode():
    uri_str = u'sip:b;transport=ws'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_TRANSPORT: 'ws'} == uri.params


def test_params_transport_wss_unicode():
    uri_str = u'sip:b;transport=wss'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_TRANSPORT: 'wss'} == uri.params


def test_params_transport_wssnew_unicode():
    uri_str = u'sip:b;transport=wssnew'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_TRANSPORT: 'wssnew'} == uri.params


def test_params_invalid_transport_unicode():
    uri_str = u'sip:b;transport=&'
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_params_invalid_transport_second_param_unicode():
    uri_str = u'sip:b;transport=&;user=phone'
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_multi_params_unicode():
    uri_str = u'sip:b;transport=tcp;user=phone'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {'transport': 'tcp', 'user': 'phone'} == uri.params


def test_params_lr_unicode():
    uri_str = u'sip:b;lr'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {'lr': True} == uri.params


def test_maddr_unicode():
    uri_str = u'sip:b;maddr=1.1.1.1'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {'maddr': ipaddress.IPv4Address('1.1.1.1')} == uri.params


def test_invalid_maddr_unicode():
    uri_str = u'sip:b;maddr=&'
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_user_param_phone_unicode():
    uri_str = u'sip:b;user=phone'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_USER: 'phone'} == uri.params


def test_user_param_ip_unicode():
    uri_str = u'sip:b;user=ip'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_USER: 'ip'} == uri.params


def test_user_param_something_unicode():
    uri_str = u'sip:b;user=something'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_USER: 'something'} == uri.params


def test_invalid_user_param_unicode():
    uri_str = u'sip:b;user=&'
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_empty_user_param_unicode():
    uri_str = u'sip:b;user='
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_ttl_param_unicode():
    uri_str = u'sip:b;ttl=1'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {PARAM_TTL: 1} == uri.params


def test_ttl_param_literal_unicode():
    uri_str = u'sip:b;ttl=a'
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_ttl_param_negative_unicode():
    uri_str = u'sip:b;ttl=-1'
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_ttl_param_too_big_unicode():
    uri_str = u'sip:b;ttl=256'
    with pytest.raises(ParamParseError):
        Uri(uri_str)


def test_some_param_unicode():
    uri_str = u'sip:b;Some=1'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {'some': '1'} == uri.params


def test_headers_unicode():
    uri_str = u'sip:b?Some=1&Another=2'
    uri = Uri(uri_str)
    assert 'b' == uri.host
    assert {'some': '1', 'another': '2'} == uri.headers


def test_invalid_uri_unicode():
    uri_str = u'a@b'
    with pytest.raises(URIParseError):
        Uri(uri_str)


def test_make_sips_unicode():
    expected_uri = Uri(u'sips:Alice@atlanta.com:8083')
    uri = Uri.make(scheme=u'sips', user=u'Alice', host=u'atlanta.com', port=8083)
    assert expected_uri == uri


def test_make_sip_unicode():
    expected_uri2 = Uri(u'sip:Alice@atlanta.com:5061')
    uri2 = Uri.make(scheme=u'sip', user=u'Alice', host=u'atlanta.com', port=u'5061')
    assert expected_uri2 == uri2


def test_make_invalid_hostname_unicode():
    with pytest.raises(HostParseError):
        Uri.make(host=u'-ab')


def test_make_invalid_part_unicode():
    with pytest.raises(SIPUriError):
        Uri.make(x=u'a-b')


def test_make_invalid_unicode():
    with pytest.raises(Exception):
        Uri.make(u'x')


def test_compare_eq_unicode():
    assert Uri(u'sip:%61lice@atlanta.com;transport=TCP') == Uri(u'sip:alice@AtLanTa.CoM;Transport=tcp')
    assert Uri(u'sip:%61lice@atlanta.com;transport=TCP') == Uri(u'sip:alice@AtLanTa.CoM;Transport=tcp')
    assert Uri(u'sip:carol@chicago.com') == Uri(u'sip:carol@chicago.com;newparam=5')
    assert Uri(u'sip:carol@chicago.com') == Uri(b'sip:carol@chicago.com;security=on')
    assert Uri(u'sip:carol@chicago.com;newparam=5') == Uri(b'sip:carol@chicago.com;security=on')
    assert Uri(u'sip:biloxi.com;transport=tcp;method=REGISTER?to=sip:bob%40biloxi.com') == \
           Uri(u'sip:biloxi.com;method=REGISTER;transport=tcp?to=sip:bob%40biloxi.com')
    assert Uri(u'sip:alice@atlanta.com?subject=project%20x&priority=urgent') == \
           Uri(b'sip:alice@atlanta.com?priority=urgent&subject=project%20x')
    assert Uri(u'sip:1.1.1.1;transport=tcp') == Uri(u'sip:1.1.1.1;transport=tcp')
    assert Uri(u'sip:[::1];transport=tcp') == Uri(u'sip:[::1];transport=tcp')
    assert Uri('sip:[::1]:5060;transport=tcp') == Uri(u'sip:[::1]:5060;transport=tcp')


def test_compare_ne_unicode():
    assert Uri(u'SIP:ALICE@AtLanTa.CoM;Transport=udp') != Uri(u'sip:alice@AtLanTa.CoM;Transport=UDP')
    assert Uri(b'sip:bob@biloxi.com') != Uri(u'sip:bob@biloxi.com:5060')
    assert Uri(b'sip:bob@biloxi.com') != Uri(u'sip:bob@biloxi.com;transport=udp')
    assert Uri(b'sip:bob@biloxi.com') != Uri(u'sip:bob@biloxi.com:6000;transport=tcp')
    assert Uri(b'sip:carol@chicago.com') != Uri(u'sip:carol@chicago.com?Subject=next%20meeting')


@pytest.mark.parametrize('uri_str', [u'sip:1.1.1.1;transport=tcp',
                                     u'sips:%20@b:5090',
                                     u'sip:b;transport=wss',
                                     u'sip:b;maddr=1.1.1.1',
                                     u'sip:b;ttl=1',
                                     u'sip:b;lr',
                                     u'sip:b;user=phone',
                                     u'sip:b;user=ip',
                                     u'sip:b;user=Some',
                                     u'sip:b;myparam=Param',
                                     u'sip:b;myparam',
                                     u'tel:b;myparam'
                                     ])
def test_assemble_unicode(uri_str):
    assert Uri(uri_str).uri == uri_str


def test_get_unicode():
    uri = Uri(u'sip:bob@1.1.1.1:5091')
    assert 'sip' == uri.get(part='scheme')
    assert 'bob' == uri.get(part='user')
    assert ipaddress.IPv4Address('1.1.1.1') == uri.get(part='host')
    assert 5091 == uri.get(part='port')


def test_user_setter_unicode():
    uri = Uri(u'sip:bob@1.1.1.1:5091')
    uri.user = u'alice'
    assert uri.user == 'alice'
    assert uri.uri == 'sip:alice@1.1.1.1:5091'


def test_host_setter_unicode():
    uri = Uri(u'sip:bob@1.1.1.1:5091')
    uri.host = u'biloxi.com'
    assert uri.host == 'biloxi.com'
    assert uri.uri == 'sip:bob@biloxi.com:5091'


def test_port_setter_unicode():
    uri = Uri(u'sip:bob@1.1.1.1:5091')
    uri.port = 5092
    assert uri.port == 5092
    assert uri.uri == 'sip:bob@1.1.1.1:5092'


def test_port_setter_none_unicode():
    uri = Uri(u'sip:bob@1.1.1.1:5091')
    uri.port = None
    assert uri.port is None
    assert uri.uri == 'sip:bob@1.1.1.1'


def test_three_params_unicode():
    uri = Uri(u'sip:carol@chicago.com;param1=value1;param2=value2;param3=value3')
    assert {'param1': 'value1', 'param2': 'value2', 'param3': 'value3'} == uri.params
