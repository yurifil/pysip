from pysip import PySIPException
from pysip.uri import PARAM_LR
from pysip.message.sip_message import SipMessage, SIPMessageParseError, NotFound, SipHeaderError, bad_request_reason
from pysip.message.hnames import MAXFORWARDS_HEADER, PROXY_REQUIRE_HEADER, RECORD_ROUTE_HEADER, ROUTE_HEADER, TO_HEADER, \
    FROM_HEADER, CALLID_HEADER, CSEQ_HEADER, ALLOW_HEADER, UNSUPPORTED_HEADER, SUPPORTED_HEADER
from pysip.message.method import OPTIONS, ACK, CANCEL
from pysip.message.hdr_maxforwards import MaxForwardsHeader
from pysip.message.hdr_route import RouteHeader
from pysip.message.hdr_opttag_list import OptTagListHeader
from pysip.message.route_set import RouteSet
from pysip.reply import ReplyOptions, AUTO
from pysip.source import is_tls


ALL_SUPPORTED = 'all_supported'
LOOSE_ROUTING = 'loose'


class ForwardOptions(object):
    def __init__(self, routing=None, nexthop=None):
        self.routing = routing
        self.nexthop = nexthop


class ValidationOptionsDetails(object):
    def __init__(self, to_tag=AUTO, scheme_validation_function=None, reply_on_options=None):
        self.to_tag = to_tag
        self.scheme_validation_function = scheme_validation_function
        self.reply_on_options = reply_on_options

    def __repr__(self):
        return f'{self.__class__.__name__}(to_tag={self.to_tag}, scheme_validation_function={self.scheme_validation_function}, reply_on_options={self.reply_on_options})'


class ProxyOptionsDetails(object):
    def __init__(self, allow=None, supported=None, check_rroute_function=None, no_validate=None, record_route_uri=None):
        self.allow = allow
        self.supported = supported
        self.check_rroute_function = check_rroute_function
        self.no_validate = no_validate
        self.record_route_uri = record_route_uri
        if self.allow is None:
            self.allow = OptTagListHeader()
        if self.supported is None:
            self.supported = OptTagListHeader()

    def __repr__(self):
        return f'{self.__class__.__name__}(allow={self.allow}, supported={self.supported})'


class ValidationOptions(object):
    def __init__(self, proxy=None, validate=None):
        if proxy is None:
            self.proxy = ProxyOptionsDetails()
        else:
            self.proxy = proxy
        if validate is None:
            self.validate = ValidationOptionsDetails()
        else:
            self.validate = validate


class CommonProxyError(PySIPException):
    pass


class CommonProxy(object):
    # TODO: not implemented yet
    @staticmethod
    def _ri_process_maddr(sip_message, proxy_params):
        # maddr processing in according to 16.4
        #
        # If the Request-URI contains a maddr parameter, the proxy MUST check to see if its value is in the set of
        # addresses or domains the proxy is configured to be responsible for.  If the Request-URI has a maddr parameter
        # with a value the proxy is responsible for, and the request was received using the port and transport indicated
        # (explicitly or by default) in the Request-URI, the proxy MUST strip the maddr and any non-default port or
        # transport parameter and continue processing as if those values had not been present in the request.
        pass

    @staticmethod
    def _check_supported(required, validation_options):
        if validation_options.proxy.supported is not None:
            intersect = required.intersect(validation_options.proxy.supported)
            if intersect == required:
                return ALL_SUPPORTED
            else:
                return required.substract(validation_options.proxy.supported)
        else:
            return required

    @staticmethod
    def _make_bad_extension(sip_message, validation_options, unsupported):
        try:
            SipMessage.parse(sip_message, [TO_HEADER, FROM_HEADER, CALLID_HEADER,CSEQ_HEADER])
            to_tag = AUTO
            if validation_options.validate.to_tag is not None:
                to_tag = validation_options.validate.to_tag
            reply_opts = ReplyOptions(420, to_tag=to_tag)
            response = sip_message.reply(reply_opts)
            response.set(UNSUPPORTED_HEADER, unsupported)
            return response
        except Exception as e:
            raise e

    @staticmethod
    def _make_reply(sip_msg, validation_options, code):
        try:
            sip_msg = SipMessage.parse(sip_msg, [TO_HEADER, FROM_HEADER, CALLID_HEADER, CSEQ_HEADER])
        except SIPMessageParseError as e:
            raise e
        to_tag = AUTO
        if validation_options.validate.to_tag is not None:
            to_tag = validation_options.validate.to_tag
        reply_opts = ReplyOptions(status=code, reason=None, to_tag=to_tag)
        return sip_msg.reply(reply_opts)

    @staticmethod
    def _make_bad_request(raw_message, validation_options, error):
        try:
            sip_msg = SipMessage.parse(raw_message, [TO_HEADER, FROM_HEADER, CALLID_HEADER, CSEQ_HEADER])
        except SIPMessageParseError as e:
            raise e
        reason = bad_request_reason(error)
        to_tag = AUTO
        if validation_options.validate.to_tag is not None:
            to_tag = validation_options.validate.to_tag
        reply_options = ReplyOptions(status=400, reason=reason, to_tag=to_tag)
        return sip_msg.reply(reply_options)

    @staticmethod
    def _maybe_reply_options(sip_msg, validation_options):
        if validation_options.validate.reply_on_options:
            return CommonProxy._make_options_reply(sip_msg, validation_options)
        return CommonProxy._make_reply(sip_msg, validation_options, 483)

    @staticmethod
    def _maybe_add_allow(validation_options, sip_message):
        if validation_options.proxy.allow is not None:
            sip_message.set(ALLOW_HEADER, validation_options.proxy.allow)

    # TODO: decide if this method should exist
    @staticmethod
    def _maybe_add_accept(validation_options, sip_message):
        pass

    # TODO: decide if this method should exist
    @staticmethod
    def _maybe_add_accept_encoding(validation_options, sip_message):
        pass

    # TODO: decide if this method should exist
    @staticmethod
    def _maybe_add_accept_language(validation_options, sip_message):
        pass

    @staticmethod
    def _maybe_add_supported(validation_options, sip_message):
        if validation_options.proxy.supported is not None:
            sip_message.set(SUPPORTED_HEADER, validation_options.proxy.supported)

    @staticmethod
    def _make_options_reply(sip_msg, validation_options):
        if validation_options.validate.to_tag is not None:
            to_tag = validation_options.validate.to_tag
        else:
            to_tag = AUTO
        reply_opts = ReplyOptions(200, to_tag=to_tag)
        # The response to an OPTIONS is constructed using the standard rules
        # for a SIP response as discussed in Section 8.2.6.
        response_200 = sip_msg.reply(reply_opts)
        # If the response to an OPTIONS is generated by a proxy server, the proxy returns a 200 (OK), listing the
        # capabilities of the server. The response does not contain a message body.
        # Allow, Accept, Accept-Encoding, Accept-Language, and Supported header fields SHOULD be present in a 200 (OK)
        # response to an OPTIONS request. If the response is generated by a proxy, the Allow header...
        CommonProxy._maybe_add_allow(validation_options, response_200)
        CommonProxy._maybe_add_accept(validation_options, response_200)
        CommonProxy._maybe_add_accept_encoding(validation_options, response_200)
        CommonProxy._maybe_add_accept_language(validation_options, response_200)
        CommonProxy._maybe_add_supported(validation_options, response_200)
        return response_200

    @staticmethod
    def _validate_proxy_params(proxy_params):
        if proxy_params.no_validate is not None and proxy_params.no_validate:
            return True
        if proxy_params.record_route_uri is not None and proxy_params.check_rroute_function is None:
            raise CommonProxyError(f'Required proxy option "check_rroute_fun"')

    @staticmethod
    def _ri_strict_route(sip_message, proxy_params):
        if proxy_params.check_rroute_function is not None and proxy_params.check_rroute_function(sip_message.ruri):
            route_hdr = sip_message.find(ROUTE_HEADER)
            if not isinstance(route_hdr, NotFound):
                last_route = route_hdr.route_set.last
                sip_message.ruri = last_route.uri
                CommonProxy._remove_last_route(sip_message)

    @staticmethod
    def _remove_last_route(sip_message):
        route_hdr = sip_message.get(ROUTE_HEADER)
        route_hdr.route_set.remove_last()
        sip_message.set(ROUTE_HEADER, route_hdr)

    @staticmethod
    def _do_validate_proxy_require(sip_message, validation_options):
        pr_hdr = sip_message.find(PROXY_REQUIRE_HEADER)
        if isinstance(pr_hdr, NotFound):
            return sip_message
        else:
            unsupported = CommonProxy._check_supported(pr_hdr, validation_options)
            if unsupported == ALL_SUPPORTED:
                return sip_message
            else:
                return CommonProxy._make_bad_extension(sip_message, validation_options, unsupported)

    @staticmethod
    def _validate_reasonable_syntax(raw_message, validation_options):
        try:
            return SipMessage.parse(raw_message, [MAXFORWARDS_HEADER, PROXY_REQUIRE_HEADER, RECORD_ROUTE_HEADER,
                                                  ROUTE_HEADER])
        except (SIPMessageParseError, SipHeaderError) as e:
            try:
                return CommonProxy._make_bad_request(raw_message, validation_options, e)
            except Exception as e:
                raise e

    @staticmethod
    def _validate_uri_scheme(sip_message, validation_options):
        """

        Args:
            sip_message (SipMessage):
            validation_options (ValidationOptions):

        Returns:

        """
        if validation_options.validate.scheme_validation_function is not None:
            if validation_options.validate.scheme_validation_function(sip_message.ruri):
                return sip_message
            else:
                return CommonProxy._make_reply(sip_message, validation_options, 416)
        else:
            return sip_message

    @staticmethod
    def _validate_max_forwards(sip_msg, validation_options):
        max_forwards = sip_msg.find(MAXFORWARDS_HEADER)
        if isinstance(max_forwards, NotFound):
            # If the request does not contain a Max-Forwards header field, this check is passed.
            return sip_msg
        elif max_forwards.maxforwards > 0:
            # If the request contains a Max-Forwards header field with a field value greater than zero, the check is
            # passed.
            return sip_msg
        elif max_forwards.maxforwards == 0:
            # If the request contains a Max-Forwards header field with a field value of zero (0), the element MUST NOT
            # forward the request.  If the request was for OPTIONS, the element MAY act as the final recipient and
            # respond per Section 11.  Otherwise, the element MUST return a 483 (Too many hops) response.
            if sip_msg.method != OPTIONS:
                return CommonProxy._make_reply(sip_msg, validation_options, 483)
            return CommonProxy._maybe_reply_options(sip_msg, validation_options)

    # TODO: not implemented yet
    @staticmethod
    def _validate_loop_detect(sip_message, validation_options):
        return sip_message

    @staticmethod
    def _validate_proxy_require(sip_message, validation_options):
        # If the request contains a Proxy-Require header field (Section 20.29) with one or more option-tags this
        # element does not understand, the element MUST return a 420 (Bad Extension) response.  The response MUST
        # include an Unsupported (Section 20.40) header field listing those option-tags the element did not
        # understand.
        if sip_message.method == ACK or sip_message.method == CANCEL:
            return sip_message
        else:
            return CommonProxy._do_validate_proxy_require(sip_message, validation_options)

    # TODO: not implemented yet
    @staticmethod
    def _validate_proxy_authorization(sip_message, validation_options):
        # If an element requires credentials before forwarding a request, the request MUST be inspected as described in
        # Section 22.3. That section also defines what the element must do if the inspection fails.
        return sip_message

    @staticmethod
    def validate_request(raw_message, validation_options):
        sip_msg = CommonProxy._validate_reasonable_syntax(raw_message, validation_options)
        sip_msg = CommonProxy._validate_uri_scheme(sip_msg, validation_options)
        sip_msg = CommonProxy._validate_max_forwards(sip_msg, validation_options)
        sip_msg = CommonProxy._validate_loop_detect(sip_msg, validation_options)
        sip_msg = CommonProxy._validate_proxy_require(sip_msg, validation_options)
        sip_msg = CommonProxy._validate_proxy_authorization(sip_msg, validation_options)
        return sip_msg

    @staticmethod
    def _ri_maybe_remove_route(sip_message, proxy_params):
        """Removes route from SIP message if necessary.

        Args:
            sip_message (SipMessage)
            proxy_params (ProxyOptionsDetails)
        """
        if proxy_params.check_rroute_function is not None:
            route_hdr = sip_message.find(ROUTE_HEADER)
            if not isinstance(route_hdr, NotFound):
                first_route = route_hdr.first
                if proxy_params.check_rroute_function(first_route.uri):
                    route_hdr.route_set.remove_first()
                    sip_message.set(ROUTE_HEADER, route_hdr)

    @staticmethod
    def process_route_info(sip_message, proxy_params):
        CommonProxy._validate_proxy_params(proxy_params)
        CommonProxy._ri_strict_route(sip_message, proxy_params)
        CommonProxy._ri_process_maddr(sip_message, proxy_params)
        CommonProxy._ri_maybe_remove_route(sip_message, proxy_params)
        return sip_message

    @staticmethod
    def _fwd_set_ruri(target, fwd_result):
        """Cleans uri in fwd result.

        Args:
            target (Uri): target uri
            fwd_result (tuple): tuple containing SipMessage instance and fwd options

        Returns:
            tuple of SipMessage instance with cleaned ruri and fwd options

        """
        # The Request-URI in the copy's start line MUST be replaced with the URI for this target.  If the URI contains
        # any parameters not allowed in a Request-URI, they MUST be removed.
        sip_msg, opts = fwd_result
        sip_msg.ruri = target.clear_not_allowed_parts('ruri')
        return sip_msg, opts

    @staticmethod
    def _fwd_max_forwards(fwd_result):
        """Decrements max-forwards value if it exists in fwd result SIP message, sets it to 70 otherwise.

        Args:
            fwd_result (tuple): tuple containing SipMessage instance and fwd options

        Returns:

        """
        sip_msg, opts = fwd_result
        try:
            mf = sip_msg.find(MAXFORWARDS_HEADER)
        except Exception as e:
            raise e
        if isinstance(mf, NotFound):
            # If the copy does not contain a Max-Forwards header field, the proxy MUST add one with a field value,
            # which SHOULD be 70.
            sip_msg.set(MAXFORWARDS_HEADER, MaxForwardsHeader(70))
        else:
            mf.decrement()
            sip_msg.set(MAXFORWARDS_HEADER, mf)
        return sip_msg, opts

    @staticmethod
    def _fwd_record_route(target, fwd_result, proxy_params):
        """

        Args:
            target:
            fwd_result:
            proxy_params (ProxyOptionsDetails):

        Returns:

        """
        sip_msg, opts = fwd_result
        if proxy_params.record_route_uri is not None:
            rr = proxy_params.record_route_uri.clear_not_allowed_parts(RECORD_ROUTE_HEADER)
            rr.set_param(PARAM_LR, True)
            rrroute = RouteHeader.make_route(rr)
            rr_set = sip_msg.find(RECORD_ROUTE_HEADER)
            if isinstance(rr_set, NotFound):
                rr_set = RouteHeader()
            rr_set.route_set.add_first(rrroute)
            sip_msg.set(RECORD_ROUTE_HEADER, rr_set)
        return sip_msg, opts

    @staticmethod
    # TODO: not implemented yet
    def _fwd_add_headers(target, fwd_result, proxy_params):
        return fwd_result

    @staticmethod
    def _fwd_postprocess_routing(target, fwd_result, proxy_params):
        return CommonProxy._maybe_strict_router_workaround(fwd_result)

    @staticmethod
    def _first_route_from_route_set(sip_msg):
        route_header = sip_msg.find(ROUTE_HEADER)
        if not isinstance(route_header, NotFound):
            if route_header.route_set.is_empty():
                return NotFound()
            return route_header.route_set.first
        return route_header

    @staticmethod
    def _maybe_strict_router_workaround(fwd_result):
        sip_msg, opts = fwd_result
        first_route = CommonProxy._first_route_from_route_set(sip_msg)
        if isinstance(first_route, NotFound) or first_route.is_loose_route():
            return fwd_result
        return CommonProxy._strict_router_workaround(fwd_result)

    @staticmethod
    def _strict_router_workaround(fwd_result):
        # The proxy MUST place the Request-URI into the Route header
        # field as the last value.
        sip_msg, opts = fwd_result
        route_hdr = sip_msg.get(ROUTE_HEADER)
        ruri_route = RouteHeader.make_route(sip_msg.ruri)
        route_hdr.route_set.add_last(ruri_route)
        # The proxy MUST then place the first Route header field value
        # into the Request-URI and remove that value from the Route
        # header field.
        sip_msg.ruri = route_hdr.route_set.first.uri
        route_hdr.route_set.remove_first()
        sip_msg.set(ROUTE_HEADER, route_hdr)
        return sip_msg, ForwardOptions(routing='strict')

    @staticmethod
    def _nexthop_scheme_is(uri_scheme, sip_msg):
        ruri_scheme_match = sip_msg.ruri.scheme == uri_scheme
        first_route = CommonProxy._first_route_from_route_set(sip_msg)
        if isinstance(first_route, NotFound):
            top_route_sceme_match = False
        else:
            top_route_sceme_match = first_route.uri.scheme == uri_scheme
        return ruri_scheme_match or top_route_sceme_match

    @staticmethod
    def _fwd_check_record_route(target, fwd_result, proxy_params):
        sip_msg, opts = fwd_result
        if proxy_params.record_route_uri is not None:
            # If the Request-URI contains a SIPS URI, or the topmost Route
            # header field value (after the post processing of bullet 6)
            # contains a SIPS URI, the URI placed into the Record-Route header
            # field MUST be a SIPS URI.
            if CommonProxy._nexthop_scheme_is('sips', sip_msg):
                if proxy_params.record_route_uri.scheme == 'sips':
                    pass
                else:
                    raise CommonProxyError(f'Record route must be sips')
            return sip_msg, opts
        else:
            # Note: No record-route is defined by proxy in this clause.
            # Furthermore, if the request was not received over TLS, the
            # proxy MUST insert a Record-Route header field.  In a similar
            # fashion, a proxy that receives a request over TLS, but
            # generates a request without a SIPS URI in the Request-URI or
            # topmost Route header field value (after the post processing of
            # bullet 6), MUST insert a Record-Route header field that is not
            # a SIPS URI.
            if sip_msg.source is not None:
                if not is_tls(sip_msg.source) and CommonProxy._nexthop_scheme_is('sips', sip_msg):
                    raise CommonProxyError(f'Record route required: sips transform')
                if is_tls(sip_msg.source) and CommonProxy._nexthop_scheme_is('sip', sip_msg):
                    raise CommonProxyError(f'Record route required: sip transform')
        return sip_msg, opts

    @staticmethod
    def _fwd_determine_nexthop(target, fwd_result, proxy_params):
        # If the proxy has reformatted the request to send to a
        # strict-routing element as described in step 6 above, the proxy
        # MUST apply those procedures to the Request-URI of the request.
        sip_msg, opts = fwd_result
        if opts.routing is not None and opts.routing == 'strict':
            opts.nexthop = sip_msg.ruri
            return sip_msg, opts
        elif opts.routing is not None and opts.routing == 'loose':
            first_route = CommonProxy._first_route_from_route_set(sip_msg)
            if isinstance(first_route, NotFound):
                next_hop_uri = sip_msg.ruri
            else:
                next_hop_uri = first_route.uri
            opts.nexthop = next_hop_uri
        return sip_msg, opts

    @staticmethod
    def forward_request(target, sip_message, proxy_params):
        CommonProxy._validate_proxy_params(proxy_params)
        fwd_result = (sip_message, ForwardOptions(routing='loose'))
        fwd_result = CommonProxy._fwd_set_ruri(target, fwd_result)
        fwd_result = CommonProxy._fwd_max_forwards(fwd_result)
        fwd_result = CommonProxy._fwd_record_route(target, fwd_result, proxy_params)
        fwd_result = CommonProxy._fwd_add_headers(target, fwd_result, proxy_params)
        fwd_result = CommonProxy._fwd_postprocess_routing(target, fwd_result, proxy_params)
        fwd_result = CommonProxy._fwd_check_record_route(target, fwd_result, proxy_params)
        fwd_result = CommonProxy._fwd_determine_nexthop(target, fwd_result, proxy_params)
        return fwd_result

