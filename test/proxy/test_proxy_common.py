from pysip.proxy.proxy_common import CommonProxy, ValidationOptions, ValidationOptionsDetails, ProxyOptionsDetails, \
    CommonProxyError
from pysip.uri.transport import Transport, UDP, TLS
from pysip.transport.parser import Parser
from pysip.source import Source
from pysip.uri.uri import Uri
from pysip.message.option_tag import OptionTag
from pysip.message.hdr_opttag_list import OptTagListHeader
from pysip.message.hdr_route import RouteHeader
from pysip.message.hdr_allow import AllowHeader
from pysip.message.sip_message import NotFound, SipHeaderError, SipMessageError, SIPMessageParseError, ALL
from pysip.message.method import INVITE, BYE, CANCEL, OPTIONS, ACK
from pysip.message.method_set import MethodSet
from pysip.message.sip_message import SipMessage
from pysip.message.hnames import ROUTE_HEADER, ALLOW_HEADER, SUPPORTED_HEADER, UNSUPPORTED_HEADER, MAXFORWARDS_HEADER, \
    RECORD_ROUTE_HEADER, make_key
from pysip.message.hdr import Header

import pytest

peer = ((127, 0, 0, 1), 5060)
udp_source = Source(peer=peer, transport=Transport(UDP))
tls_source = Source(peer=peer, transport=Transport(TLS))


def forward_request(target, raw_msg, opts):
    sip_msg = CommonProxy.validate_request(raw_msg, ValidationOptions(proxy=opts))
    return CommonProxy.forward_request(Uri(target), sip_msg, opts)


def process_route_info(message, options):
    sip_msg = CommonProxy.validate_request(message, options)
    CommonProxy.process_route_info(sip_msg, options.proxy)
    return sip_msg


def raw_message(msg_string):
    p = Parser.new_dgram(msg_string)
    sip_msg, rest = Parser.parse(p)
    return sip_msg


def rebuild_sip_msg(sip_msg):
    sip_msg_str = sip_msg.serialize()
    p = Parser.new_dgram(sip_msg_str)
    msg, rest = Parser.parse(p)
    return SipMessage.parse(msg, ALL)


def test_stateless_proxy_forward_to_proxy():
    def check_rroute_fun(uri):
        return uri == Uri(this_proxy_uri)

    bob_uri = 'sip:bob@biloxi.com'
    this_proxy_uri = 'sip:this.proxy.org'
    next_proxy_uri = 'sip:next.proxy.org'
    msg = 'INVITE ' + bob_uri + ' SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nRoute: <' + this_proxy_uri + '>' + \
          '\r\nRoute: <' + next_proxy_uri + '>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    proxy_opts = ProxyOptionsDetails(check_rroute_function=check_rroute_fun)
    options = ValidationOptions(proxy=proxy_opts)
    sip_msg = process_route_info(raw_message(msg), options)
    target = Uri(next_proxy_uri)
    sip_msg, opts = CommonProxy.forward_request(target, sip_msg, proxy_opts)
    expected_route_hdr = RouteHeader(f'<{next_proxy_uri}>')
    assert target == opts.nexthop
    assert expected_route_hdr == sip_msg.find(ROUTE_HEADER)


def test_stateless_proxy_forward_to_ua():
    def check_rroute_fun(uri):
        return uri == Uri(this_proxy_uri)

    bob_uri = 'sip:bob@biloxi.com'
    this_proxy_uri = 'sip:this.proxy.org'
    msg = 'INVITE ' + bob_uri + ' SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nRoute: <' + this_proxy_uri + '>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    proxy_opts = ProxyOptionsDetails(check_rroute_function=check_rroute_fun)
    options = ValidationOptions(proxy=proxy_opts)
    sip_msg = process_route_info(raw_message(msg), options)
    target = Uri(bob_uri)
    sip_msg, opts = CommonProxy.forward_request(target, sip_msg, proxy_opts)
    assert target == opts.nexthop
    assert isinstance(sip_msg.find(ROUTE_HEADER), NotFound)


def test_request_validation_success():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    CommonProxy.validate_request(raw_message(msg), ValidationOptions())


def test_request_validation_success_no_maxforwards():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    CommonProxy.validate_request(raw_message(msg), ValidationOptions())


def test_request_validation_bad_maxforwards():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: xyz' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = CommonProxy.validate_request(raw_message(msg), ValidationOptions())
    assert sip_msg.status == 400
    assert 'max-forwards' in sip_msg.reason.lower()


def test_request_validation_unsupported_scheme():
    def scheme_validation_function(value):
        return False

    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = CommonProxy.validate_request(raw_message(msg),
                                           ValidationOptions(validate=ValidationOptionsDetails(scheme_validation_function=scheme_validation_function)))
    assert sip_msg.status == 416
    assert 'unsupported' in sip_msg.reason.lower() and 'uri' in sip_msg.reason.lower() and \
           'scheme' in sip_msg.reason.lower()


def test_request_validation_unsupported_scheme_cannot_reply():
    # Invalid message: no To header
    def scheme_validation_function(value):
        return False
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    with pytest.raises(SipHeaderError):
        CommonProxy.validate_request(raw_message(msg), ValidationOptions(validate=ValidationOptionsDetails(scheme_validation_function=scheme_validation_function)))


def test_request_validation_no_resp_to():
    # Invalid message: invalid maxforwards and no To header
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: xyz' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    with pytest.raises(SipHeaderError):
        CommonProxy.validate_request(raw_message(msg), ValidationOptions())


def test_request_validation_no_resp_from():
    # Invalid message: invalid maxforwards and no From header
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: xyz' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    with pytest.raises(SipHeaderError):
        CommonProxy.validate_request(raw_message(msg), ValidationOptions())


def test_request_validation_maxforwards_is_zero():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 0' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = CommonProxy.validate_request(raw_message(msg), ValidationOptions())
    assert sip_msg.status == 483
    assert 'too many hops' == sip_msg.reason.lower()


def test_request_validation_maxforwards_is_zero_options():
    msg = 'OPTIONS sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 0' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 OPTIONS' + \
          '\r\n\r\n'
    sip_msg = CommonProxy.validate_request(raw_message(msg), ValidationOptions())
    assert sip_msg.status == 483
    assert 'too many hops' == sip_msg.reason.lower()


def test_request_validation_maxforwards_is_zero_options_reply():
    msg = 'OPTIONS sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 0' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 OPTIONS' + \
          '\r\n\r\n'
    allow_methods = AllowHeader(MethodSet([INVITE, ACK, OPTIONS, CANCEL, BYE]))
    supported_list = OptTagListHeader.from_list([OptionTag('100rel'), OptionTag('timers')])
    options = ValidationOptions(validate=ValidationOptionsDetails(reply_on_options=True),
                                proxy=ProxyOptionsDetails(supported=supported_list, allow=allow_methods))
    response = CommonProxy.validate_request(raw_message(msg), options)
    assert response.status == 200
    assert response.reason == 'OK'
    resp_raw_message = raw_message(response.serialize())
    sip_response = SipMessage.parse(resp_raw_message, [ALLOW_HEADER, SUPPORTED_HEADER])
    assert allow_methods == sip_response.get(ALLOW_HEADER)
    assert sip_response.get(SUPPORTED_HEADER) == supported_list


def test_request_validation_maxforwards_is_zero_options_reply_no_allow():
    msg = 'OPTIONS sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 0' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 OPTIONS\r\n\r\n'
    options = ValidationOptions(validate=ValidationOptionsDetails(reply_on_options=True))
    response = CommonProxy.validate_request(raw_message(msg), options)
    assert response.status == 200
    resp_raw_message = raw_message(response.serialize())
    assert resp_raw_message.get(ALLOW_HEADER) == Header(ALLOW_HEADER)


def test_request_validation_proxy_require():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nProxy-Require: gin' + \
          '\r\n\r\n'
    unsupported = OptTagListHeader.from_list([OptionTag('gin')])
    response = CommonProxy.validate_request(raw_message(msg), ValidationOptions())
    assert response.status == 420
    assert response.reason.lower() == 'bad extension'
    resp_raw_message = raw_message(response.serialize())
    sip_response = SipMessage.parse(resp_raw_message, [UNSUPPORTED_HEADER])
    assert sip_response.get(UNSUPPORTED_HEADER) == unsupported


def test_request_validation_proxy_require_no_required():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nProxy-Require: gin' + \
          '\r\n\r\n'
    unsupported = OptTagListHeader.from_list([OptionTag('gin')])
    supported = OptTagListHeader.from_list([OptionTag('100rel'), OptionTag('timers')])
    options = ValidationOptions(proxy=ProxyOptionsDetails(supported=supported))
    response = CommonProxy.validate_request(raw_message(msg), options)
    assert response.status == 420
    assert response.reason.lower() == 'bad extension'
    resp_raw_message = raw_message(response.serialize())
    sip_response = SipMessage.parse(resp_raw_message, [UNSUPPORTED_HEADER])
    assert sip_response.get(UNSUPPORTED_HEADER) == unsupported


def test_request_validation_proxy_require_all_supported():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nProxy-Require: gin' + \
          '\r\n\r\n'
    supported = OptTagListHeader.from_list([OptionTag('gin')])
    options = ValidationOptions(proxy=ProxyOptionsDetails(supported=supported))
    # no errors while validating:
    response = CommonProxy.validate_request(raw_message(msg), options)
    assert isinstance(response, SipMessage)


def test_request_validation_proxy_require_cannot_reply():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nProxy-Require: gin' + \
          '\r\n\r\n'
    with pytest.raises(SipHeaderError):
        CommonProxy.validate_request(raw_message(msg), ValidationOptions())


def test_proxy_params_check_no_check_fun():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    this_proxy_uri = Uri('sip:this.proxy.org')
    options = ValidationOptions(proxy=ProxyOptionsDetails(record_route_uri=this_proxy_uri))
    with pytest.raises(CommonProxyError):
        process_route_info(raw_message(msg), options)


def test_proxy_params_check_no_validate():
    msg = 'INVITE sip:bob@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    this_proxy_uri = Uri('sip:this.proxy.org')
    options = ValidationOptions(proxy=ProxyOptionsDetails(record_route_uri=this_proxy_uri, no_validate=True))
    # No exceptions raised:
    process_route_info(raw_message(msg), options)


def test_process_route_info_strict_routing():
    def check_rroute_fun(uri):
        return Uri(this_proxy_uri) == uri
    bob_uri = 'sip:bob@biloxi.com'
    this_proxy_uri = 'sip:this.proxy.org'
    next_proxy_uri = 'sip:next.proxy.org'
    msg = 'INVITE ' + this_proxy_uri + ' SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nRoute: <' + next_proxy_uri + '>' + \
          '\r\nRoute: <' + bob_uri + '>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    options = ValidationOptions(proxy=ProxyOptionsDetails(check_rroute_function=check_rroute_fun))
    sip_msg = process_route_info(raw_message(msg), options)
    sip_msg_rebuilt = rebuild_sip_msg(sip_msg)
    assert sip_msg_rebuilt.ruri == Uri(bob_uri)
    expected_route_hdr = RouteHeader(next_proxy_uri)
    assert expected_route_hdr == sip_msg_rebuilt.get(ROUTE_HEADER)


def test_process_route_info_loose_routing():
    def check_rroute_fun(uri):
        return Uri(this_proxy_uri) == uri
    bob_uri = 'sip:bob@biloxi.com'
    this_proxy_uri = 'sip:this.proxy.org'
    next_proxy_uri = 'sip:next.proxy.org'
    msg = 'INVITE ' + bob_uri + ' SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nRoute: <' + this_proxy_uri + '>' + \
          '\r\nRoute: <' + next_proxy_uri + '>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    options = ValidationOptions(proxy=ProxyOptionsDetails(check_rroute_function=check_rroute_fun))
    sip_msg = process_route_info(raw_message(msg), options)
    sip_msg_rebuilt = rebuild_sip_msg(sip_msg)
    assert sip_msg_rebuilt.ruri == Uri(bob_uri)
    expected_route_hdr = RouteHeader(next_proxy_uri)
    assert expected_route_hdr == sip_msg_rebuilt.get(ROUTE_HEADER)


def test_process_route_info_no_rr_checker():
    bob_uri = 'sip:bob@biloxi.com'
    this_proxy_uri = 'sip:this.proxy.org'
    next_proxy_uri = 'sip:next.proxy.org'
    msg = 'INVITE ' + bob_uri + ' SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nRoute: <' + this_proxy_uri + '>' + \
          '\r\nRoute: <' + next_proxy_uri + '>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = process_route_info(raw_message(msg), ValidationOptions())
    sip_msg = rebuild_sip_msg(sip_msg)
    assert sip_msg.ruri == Uri(bob_uri)
    assert sip_msg.get(ROUTE_HEADER) == RouteHeader.make(f'<{this_proxy_uri}>,<{next_proxy_uri}>')


def test_process_route_info_no_routes_loose_route():
    bob_uri = 'sip:bob@biloxi.com'
    msg = 'INVITE ' + bob_uri + ' SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    options = ValidationOptions(proxy=ProxyOptionsDetails(check_rroute_function=lambda x: False))
    sip_msg = process_route_info(raw_message(msg), options)
    sip_msg = rebuild_sip_msg(sip_msg)
    assert sip_msg.ruri == Uri(bob_uri)
    assert isinstance(sip_msg.find(ROUTE_HEADER), NotFound)


def test_process_route_info_no_routes_strict_route():
    bob_uri = 'sip:bob@biloxi.com'
    msg = 'INVITE ' + bob_uri + ' SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    options = ValidationOptions(proxy=ProxyOptionsDetails(check_rroute_function=lambda x: True))
    sip_msg = process_route_info(raw_message(msg), options)
    sip_msg = rebuild_sip_msg(sip_msg)
    assert sip_msg.ruri == Uri(bob_uri)
    assert isinstance(sip_msg.find(ROUTE_HEADER), NotFound)


def test_forward_request_set_ruri():
    bob_uri = 'sip:bob@biloxi.com'
    msg = 'INVITE ' + bob_uri + ' SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: 70' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg, options = forward_request(bob_uri, raw_message(msg), ProxyOptionsDetails())
    expected_nexthop = Uri(bob_uri)
    assert options.nexthop == expected_nexthop
    sip_msg = rebuild_sip_msg(sip_msg)
    assert sip_msg.get(MAXFORWARDS_HEADER).maxforwards == 69
    assert sip_msg.ruri == Uri(bob_uri)


def test_forward_request_add_maxforwars():
    bob_uri = 'sip:bob@biloxi.com'
    msg = 'INVITE ' + bob_uri + ' SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg, options = forward_request(bob_uri, raw_message(msg), ProxyOptionsDetails())
    sip_msg = rebuild_sip_msg(sip_msg)
    assert sip_msg.get(MAXFORWARDS_HEADER).maxforwards == 70


def test_forward_request_invalid_maxforwars():
    bob_uri = 'sip:bob@biloxi.com'
    msg = 'INVITE ' + bob_uri + ' SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nMax-Forwards: x' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg = SipMessage.parse(raw_message(msg), [])
    with pytest.raises(SipHeaderError) as exc:
        CommonProxy.forward_request(Uri(bob_uri), sip_msg, ProxyOptionsDetails())
    assert 'max-forwards' in str(exc.value).lower()


def test_forward_request_record_route_add():
    bob_uri = 'sip:bob@biloxi.com'
    msg = 'INVITE sip:bobby-online@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    this_proxy_uri = Uri('sip:this.proxy.org')
    this_proxy_uri.set_param('lr', True)
    proxy_opts = ProxyOptionsDetails(record_route_uri=this_proxy_uri,
                                     check_rroute_function=lambda x: x == this_proxy_uri)
    sip_msg, opts = forward_request(bob_uri, raw_message(msg), proxy_opts)
    sip_msg = rebuild_sip_msg(sip_msg)
    record_route_hdr = sip_msg.get(RECORD_ROUTE_HEADER)
    assert record_route_hdr.first.uri == this_proxy_uri


def test_forward_request_record_route_append():
    bob_uri = 'sip:bob@biloxi.com'
    another_proxy_uri = 'sip:another.proxy.org;lr'
    this_proxy_uri = Uri('sip:this.proxy.org')
    msg = 'INVITE sip:bobby-online@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nRecord-Route: <' + another_proxy_uri + '>' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    proxy_opts = ProxyOptionsDetails(record_route_uri=this_proxy_uri,
                                     check_rroute_function=lambda x: this_proxy_uri == x)
    sip_msg, opts = forward_request(bob_uri, raw_message(msg), proxy_opts)
    sip_msg = rebuild_sip_msg(sip_msg)
    record_route_hdr = sip_msg.get(RECORD_ROUTE_HEADER)
    first_rr_uri = record_route_hdr.first.uri
    last_rr_uri = record_route_hdr.last.uri
    this_proxy_uri.set_param('lr', True)
    assert first_rr_uri == this_proxy_uri
    assert last_rr_uri == Uri(another_proxy_uri)


def test_forward_request_record_route_append_sips():
    # If the Request-URI contains a SIPS URI, or the topmost Route header field value (after the post processing of
    # bullet 6) contains a SIPS URI, the URI placed into the Record-Route header field MUST be a SIPS URI.
    bob_uri = 'sips:bob@biloxi.com'
    another_proxy_uri = 'sip:another.proxy.org;lr'
    msg = 'INVITE sip:bobby-online@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nRecord-Route: <' + another_proxy_uri + '>' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    this_proxy_uri = Uri('sips:this.proxy.org')
    proxy_opts = ProxyOptionsDetails(record_route_uri=this_proxy_uri,
                                     check_rroute_function=lambda x: x == this_proxy_uri)
    forward_request(bob_uri, raw_message(msg), proxy_opts)


def test_forward_request_record_route_append_not_sips():
    # If the Request-URI contains a SIPS URI, or the topmost Route header field value (after the post processing of
    # bullet 6) contains a SIPS URI, the URI placed into the Record-Route header field MUST be a SIPS URI.
    bob_uri = 'sips:bob@biloxi.com'
    another_proxy_uri = 'sip:another.proxy.org;lr'
    msg = 'INVITE sip:bobby-online@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nRecord-Route: <' + another_proxy_uri + '>' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    this_proxy_uri = Uri('sip:this.proxy.org')
    proxy_opts = ProxyOptionsDetails(record_route_uri=this_proxy_uri,
                                     check_rroute_function=lambda x: x == this_proxy_uri)
    with pytest.raises(CommonProxyError) as exc:
        forward_request(bob_uri, raw_message(msg), proxy_opts)
    assert 'record route must be sips' == str(exc.value).lower()


def test_forward_request_no_rr_sip_to_sips():
    bob_uri = 'sips:bob@biloxi.com'
    another_proxy_uri = 'sip:another.proxy.org;lr'
    msg = 'INVITE sip:bobby-online@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nRecord-Route: <' + another_proxy_uri + '>' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    raw_msg = raw_message(msg)
    raw_msg.source = udp_source
    with pytest.raises(CommonProxyError) as exc:
        forward_request(bob_uri, raw_msg, ProxyOptionsDetails())
    assert 'sips transform' in str(exc.value).lower()


def test_forward_request_no_rr_sips_to_sip():
    # If the Request-URI contains a SIPS URI, or the topmost Route header field value (after the post processing of
    # bullet 6) contains a SIPS URI, the URI placed into the Record-Route header field MUST be a SIPS URI.
    bob_uri = 'sip:bob@biloxi.com'
    another_proxy_uri = 'sip:another.proxy.org;lr'
    msg = 'INVITE sip:bobby-online@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nRecord-Route: <' + another_proxy_uri + '>' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    raw_msg = raw_message(msg)
    raw_msg.source = tls_source
    with pytest.raises(CommonProxyError) as exc:
        forward_request(bob_uri, raw_msg, ProxyOptionsDetails())
    assert 'sip transform' in str(exc.value).lower()


def test_forward_request_no_rr_sips_to_sip_by_route():
    # Check invariant: Either Record-route is added or nexthop cannot be sip.
    bob_uri = 'sips:bob@biloxi.com'
    another_proxy_uri = 'sip:another.proxy.org;lr'
    msg = 'INVITE sip:bobby-online@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nRoute: <' + another_proxy_uri + '>' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    raw_msg = raw_message(msg)
    raw_msg.source = tls_source
    with pytest.raises(CommonProxyError) as exc:
        forward_request(bob_uri, raw_msg, ProxyOptionsDetails())
    assert 'sip transform' in str(exc.value).lower()


def test_forward_request_no_rr_sips_to_sips():
    bob_uri = 'sips:bob@biloxi.com'
    msg = 'INVITE sip:bobby-online@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    raw_msg = raw_message(msg)
    raw_msg.source = tls_source
    sip_msg, opts = forward_request(bob_uri, raw_msg, ProxyOptionsDetails())
    sip_msg = rebuild_sip_msg(sip_msg)
    assert isinstance(sip_msg.find(RECORD_ROUTE_HEADER), NotFound)


def test_forward_request_to_strict_router():
    bob_uri = 'sip:bob@biloxi.com'
    strict_router_uri = 'sip:stict.proxy.org'
    msg = 'INVITE sip:bobby-online@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nRoute: <' + strict_router_uri + '>' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg, opts = forward_request(bob_uri, raw_message(msg), ProxyOptionsDetails())
    expected_nexthop = Uri(strict_router_uri)
    assert opts.nexthop == expected_nexthop
    sip_msg = rebuild_sip_msg(sip_msg)
    assert sip_msg.ruri == Uri(strict_router_uri)
    route_hdr = sip_msg.get(ROUTE_HEADER)
    assert route_hdr.last.uri == Uri(bob_uri)


def test_forward_request_to_loose_router():
    bob_uri = 'sip:bob@biloxi.com'
    loose_route_uri = 'sip:stict.proxy.org;lr'
    msg = 'INVITE sip:bobby-online@biloxi.com SIP/2.0' + \
          '\r\nVia: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds' + \
          '\r\nVia: SIP/2.0/UDP bigbox3.site3.atlanta.com' + \
          '\r\nTo: Bob <sip:bob@biloxi.com>' + \
          '\r\nFrom: Alice <sip:alice@atlanta.com>;tag=1928301774' + \
          '\r\nCall-ID: a84b4c76e66710@pc33.atlanta.com' + \
          '\r\nCSeq: 314159 INVITE' + \
          '\r\nContact: <sip:alice@pc33.atlanta.com>' + \
          '\r\nContent-Type: application/sdp' + \
          '\r\nRoute: <' + loose_route_uri + '>' + \
          '\r\nContent-Length: 4' + \
          '\r\n\r\nTest'
    sip_msg, opts = forward_request(bob_uri, raw_message(msg), ProxyOptionsDetails())
    expected_nexthop = Uri(loose_route_uri)
    assert opts.nexthop == expected_nexthop
    sip_msg = rebuild_sip_msg(sip_msg)
    assert sip_msg.ruri == Uri(bob_uri)
