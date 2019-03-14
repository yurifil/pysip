from pysip import PySIPException
from pysip.message.hparams import HParams, HParamNotFound
from pysip.message.branch import assemble as assemble_branch, Branch
from pysip.uri.uri_parser import SIPUriParserUnicode
from pysip.uri import PARAM_TTL, PARAM_MADDR, PARAM_RECEIVED, PARAM_BRANCH, PARAM_RPORT
from ipaddress import IPv4Address, IPv6Address
from pysip.message.parser_aux import parse_token, parse_slash, parse_non_negative_integer
from pysip.uri.transport import Transport
from pysip.message.hdr import Header, BaseSipHeader
import re
import ipaddress


PARSER = SIPUriParserUnicode()


class SentProtocol(object):
    """Simple container for VIA sent protocol data

    Args:
        name (str): protocol name.
        version (str): protocol version.
        transport (:obj:Transport): transport protocol.

    Attributes:
        name (str): protocol name.
        version (str): protocol version.
        transport (:obj:Transport): transport protocol.
    """
    def __init__(self, name=None, version=None, transport=None):
        self.name = name
        self.version = version
        self.transport = transport

    def __eq__(self, other):
        if isinstance(other, SentProtocol):
            return self.name == other.name and self.version == other.version and self.transport == other.transport
        return NotImplemented

    def __bool__(self):
        return self.name is not None or self.version is not None or self.transport is not None

    def __repr__(self):
        return f'{self.name}/{self.version}/{self.transport}'


class SentBy(object):
    """Simple container for VIA sent by data

    Args:
        host (:obj:ipaddress || str): parsed host.
        port (int, optional): parsed port.

    Attributes:
        host (:obj:ipaddress || str): parsed host.
        port (int, optional): parsed port.
    """
    def __init__(self, host=None, port=None):
        self.host = host
        self.port = port

    def __eq__(self, other):
        if isinstance(other, SentBy):
            return self.host == other.host and self.port == other.port
        return NotImplemented

    def __bool__(self):
        return self.host is not None

    def __repr__(self):
        if self.port:
            port = f':{self.port}'
        else:
            port = ''
        host = self.host
        if isinstance(self.host, IPv6Address):
            host = f'[{self.host}]'
        return f'{host}{port}'


class ViaHeaderError(PySIPException):
    pass


class ViaHeader(BaseSipHeader):
    """
    Args:
        via (:obj: Header || str): VIA header to be parsed.

    Attributes:
        sent_protocol (:obj:SentProtocol): parsed sent protocol section of VIA header.
        _sent_by (:obj:SentBy): parsed sent by section of VIA header.
        hparams (:obj:HParams): parsed via parameters.
    """
    SENT_BY_RX = re.compile(r'(.*?)(;.*)')
    IPV6_RX = re.compile(r'(\[.+?\])')

    def __init__(self, via=None):
        self.sent_protocol = dict()
        self._sent_by = dict()
        self.hparams = HParams()
        if via is not None:
            if isinstance(via, str):
                via_str = via
            else:
                raise ViaHeaderError(f'Cannot initialize ViaHeader: via should be type str, not {type(via)}')
            protocol_name, protocol_version, transport, sent_by_host, sent_by_port, via_params = \
                self.parse_via(via_str)
            self.sent_protocol = SentProtocol(name=protocol_name, version=protocol_version, transport=transport)
            self._sent_by = SentBy(host=sent_by_host, port=sent_by_port)
            self.hparams = via_params

    @staticmethod
    def parse(via_hdr):
        return ViaHeader.topmost_via(via_hdr)

    def get_raw_param(self, param_name):
        """Get raw parameter value.

        Args:
            param_name (str): parameter name.

        Returns:
            :obj: raw parameter value.
        """
        return self.hparams.find_raw(param_name)

    @property
    def ttl(self):
        """TTL value"""
        ttl = self.hparams.find(PARAM_TTL)
        if not isinstance(ttl, HParamNotFound):
            return ttl
        return None

    @ttl.setter
    def ttl(self, value):
        ttl, parsed_value = self.parse_known_param_fun(PARAM_TTL, value)
        self.hparams.set(PARAM_TTL, parsed_value, PARAM_TTL, value)

    @property
    def sent_by(self):
        """Sent by host and port values.

        Returns:
            :obj:SentBy: Parsed sent by section of VIA header. If port is not specified (is None), default port for transport is
            returned.
        """
        ret_val = SentBy(host=self._sent_by.host, port=self._sent_by.port)
        if ret_val.port is None:
            ret_val.port = Transport.default_port(self.sent_protocol.transport)
        return ret_val

    @sent_by.setter
    def sent_by(self, value):
        if not isinstance(value, SentBy):
            raise NotImplemented
        self._sent_by = value

    @property
    def branch(self):
        """Branch value.

        Returns:
            :obj:Branch: Branch parameter. If no branch is specified, None is returned.
        """
        branch = self.hparams.find(PARAM_BRANCH)
        if isinstance(branch, HParamNotFound):
            return None
        return branch

    @branch.setter
    def branch(self, value):
        self.hparams.set(PARAM_BRANCH, value, 'Branch', assemble_branch(branch=value))

    @property
    def received(self):
        """Received value.

        Returns:
            :obj:ipaddress || str: Received parameter. If no received is specified, None is returned.
        """
        received = self.hparams.find(PARAM_RECEIVED)
        print(f'received: ', received)
        if not isinstance(received, HParamNotFound):
            return received
        return None

    @property
    def maddr(self):
        """maddr value."""
        maddr = self.hparams.find(PARAM_MADDR)
        if not isinstance(maddr, HParamNotFound):
            return maddr
        return None

    @staticmethod
    def assemble_param_value(name, value):
        """Get parameter's value string representation.

        Args:
            name (str): parameter name.
            value (:obj:): parameter value.

        Returns:
            str: Value of specified parameter.
        """
        if name == PARAM_RPORT and value is True:
            value = None
        elif name == PARAM_BRANCH:
            value = assemble_branch(value)
        return value

    def set_param(self, name, value):
        """Sets RECEIVED or RPORT parameter.

        Args:
            name (str): parameter name
            value (str || int): valid host str or rport int in range 1..65535

        Raises:
            ViaHeaderError if RECEIVED value is not a valid IPv4 or IPv6 host.
            ViaHeaderError if RPORT is not a integer in range 1..65535.
        """
        if name == PARAM_RECEIVED:
            host = value
            if isinstance(value, str):
                try:
                    host = PARSER.parse_host(value)
                except Exception as e:
                    raise ViaHeaderError(f'Cannot set Via param {name}={value}: invalid host {e}')
            if isinstance(value, IPv6Address):
                value = f'[{value}]'
            if isinstance(host, IPv4Address) or isinstance(host, IPv6Address):
                self.hparams.set(PARAM_RECEIVED, host, PARAM_RECEIVED, ViaHeader.assemble_param_value(PARAM_RECEIVED, value))
            else:
                raise ViaHeaderError(f'Cannot set Via RECEIVED param {name}={value}: invalid IPv4 or IPv6 host')
        elif name == PARAM_RPORT:
            if (isinstance(value, bool) and value is False) or \
                    (not isinstance(value, int) or value < 1 or value > 65535):
                raise ViaHeaderError(f'Cannot set Via RPORT param {name}={value}: invalid rport')
            self.hparams.set(PARAM_RPORT, value, PARAM_RPORT, ViaHeader.assemble_param_value(PARAM_RPORT, value))

    @staticmethod
    def parse_transport(transport_str):
        """Parse transport from string that starts with transport description.

        Args:
            transport_str (str): tail of Via header value that contains transport information.

        Returns:
            :obj:tuple of :obj:Transport, :obj:str
        """
        transport, rest = parse_token(transport_str)
        try:
            transport = Transport(transport)
            return transport, rest
        except Exception as e:
            raise e

    @staticmethod
    def parse_sent_protocol(via_string):
        """Parse sent protocol from Via header string.

        Args:
            via_string (str): Via header value.

        Returns:
            :obj:tuple of str:protocol_name, str:protocol_version, :obj:Transport, str:rest: parsed protocol name,
            protocol version, protocol transport and unparsed rest.
        """
        try:
            protocol_name, rest = parse_token(via_string)
        except Exception as e:
            raise ViaHeaderError(f'Cannot parse protocol name from Via header "{via_string}": {e}')
        try:
            slash, rest = parse_slash(rest)
            protocol_version, rest = parse_token(rest)
        except Exception as e:
            raise ViaHeaderError(f'Cannot parse protocol version from Via header "{via_string}": {e}')
        try:
            slash, rest = parse_slash(rest)
            transport, rest = ViaHeader.parse_transport(rest)
        except Exception as e:
            raise ViaHeaderError(f'Cannot parse transport from Via header "{via_string}": {e}')
        return protocol_name, protocol_version, transport, rest

    @staticmethod
    def parse_via(via_str):
        """Parse Via header.

        Args:
            via_str (str): Via header value.

        Returns:
            str:protocol_name, str:protocol_version, Transport:transport, str:sent_by_host, str:sent_by_port, HParams:via_params

        """
        try:
            protocol_name, protocol_version, transport, rest = ViaHeader.parse_sent_protocol(via_str)
            sent_by_host, sent_by_port, rest = ViaHeader.parse_sent_by(rest.strip())
            via_params, rest = ViaHeader.parse_via_params(rest.strip())
            return protocol_name, protocol_version, transport, sent_by_host, sent_by_port, via_params
        except Exception as e:
            raise e

    @staticmethod
    def parse_sent_by(sentby_str):
        """Parses Via header sent by value out of string.

        Args:
            sentby_str (str): string that starts with sent_by host and port

        Returns:
            host, int:port and unparsed str:rest

        Raises:
            ViaHeaderError if sentby_str host and port are not rfc compliant.
        """
        try:
            rx_match = ViaHeader.SENT_BY_RX.match(sentby_str)
            if rx_match:
                host_port, rest = rx_match.group(1).strip(), rx_match.group(2)
            else:
                host_port, rest = sentby_str.strip(), ''
            print(f'ViaHeader.parse_sent_by({sentby_str}): hostport {host_port}')
            h, p = ViaHeader.parse_sent_by_host_port(host_port)
            return h, p, rest
        except Exception as e:
            raise ViaHeaderError(f'Cannot parse Via sent by parameter from {sentby_str}: {e}')

    @staticmethod
    def parse_sent_by_host_port(host_port):
        """Parses Via header sent by host and port values out of string.

        Args:
            host_port (str)

        Returns:
            :obj:str||ipaddress:host, int:port

        Raises:
            ViaHeaderError if sentby_str host and port are not rfc compliant.
        """
        print(f'ViaHeader.parse_sent_by_host_port({host_port})')
        try:
            return PARSER.parse_host(host_port), PARSER.parse_port(host_port)
        except Exception as e:
            raise ViaHeaderError(f'Cannot parse via header host and port {host_port}: {e}')

    @staticmethod
    def parse_via_params(via_params_str):
        """Parses Via header parameters out of string.

        Args:
            via_params_str (str)

        Returns:
            :obj:HParams: header parameters container with parsed parameters.

        Raises:
            ViaHeaderError if parameters cannot be parsed.
        """
        if not via_params_str or not via_params_str.startswith(';'):
            return HParams(), via_params_str
        else:
            hparams = HParams()
            try:
                hparams.parse_raw(via_params_str[1:])
            except Exception as e:
                raise ViaHeaderError(f'Cannot parse Via params from {via_params_str}: {e}')
            hparams.parse_known(known_function=ViaHeader.parse_known_param_fun)
            rest = via_params_str.lstrip(hparams.assemble())
            return hparams, rest

    @staticmethod
    def parse_known_param_fun(param, value):
        """Filtering function that is passed as argument to HParams.parse_known method.

        Args:
            param (str): parameter name.
            value (str): parameter value.

        Returns:
            str:parsed_name, :obj:parsed_value
            None, None: if lowercase parameter name is not in (PARAM_TTL, PARAM_RECEIVED, PARAM_MADDR, PARAM_BRANCH, PARAM_RPORT)
        """
        if param == PARAM_TTL:
            ttl, rest = parse_non_negative_integer(value.strip())
            if rest or ttl < 0 or ttl > 255:
                raise ViaHeaderError(f'Cannot parse via header TTL {param}={value}: value should be 0..255 integer')
            return PARAM_TTL, ttl
        elif param == PARAM_RECEIVED:
            try:
                host = PARSER.parse_host(value)
            except Exception as e:
                raise ViaHeaderError(f'Cannot parse Via RECEIVED {param}={value}: invalid host {e}')
            if isinstance(host, IPv4Address) or isinstance(host, IPv6Address):
                return PARAM_RECEIVED, host
            else:
                raise ViaHeaderError(f'Cannot set Via RECEIVED {param}={value}: invalid IPv4 or IPv6 host')
        elif param == PARAM_MADDR:
            try:
                host = PARSER.parse_host(value)
            except Exception as e:
                raise ViaHeaderError(f'Cannot parse Via MADDR {param}={value}: invalid host {e}')
            return PARAM_MADDR, host
        elif param == PARAM_BRANCH:
            try:
                branch, rest = parse_token(value)
                if rest:
                    raise ViaHeaderError(f'Cannot parse Via BRANCH {param}={value}: value should be token')
                return PARAM_BRANCH, Branch(branch)
            except Exception as e:
                raise ViaHeaderError(f'Cannot parse Via BRANCH {param}={value}: {e}')
        elif param == PARAM_RPORT:
            if value is None:
                port, rest = True, ''
            else:
                port, rest = parse_non_negative_integer(value)
            if rest or (port is not None and (port <= 0 or port > 65535)):
                raise ViaHeaderError(f'Cannot parse via header RPORT {param}={value}: value should be 1..65535 integer')
            return PARAM_RPORT, port
        else:
            return None, None

    @staticmethod
    def topmost_via(via_hdr):
        """Makes a ViaHeader instance from Header object's topmost value.

        Args:
            via_hdr (obj:Header): header to be parsed.

        Returns:
            obj:ViaHeader: Via header parsed from first element of via_hdr object.

        Raises:
            ViaHeaderError if via_hdr parameter is not a Header object or if via_hdr object doesn't contain any values.

        """
        if not isinstance(via_hdr, Header):
            raise ViaHeaderError(f'Cannot parse topmost via: via_header should be of type Header not {type(via_hdr)}')
        if not via_hdr.values:
            raise ViaHeaderError(f'Cannot parse topmost via: no via')
        return ViaHeader(via_hdr.values[0])

    @staticmethod
    def make_param_key(hparams):
        """Helper function that returns comparable object of reduced HParams.

        Args:
            hparams (obj:HParams): header parameters container

        Returns:
            :obj:list: list of param_name, param_value pairs that can be compared to similar list.

        """
        params_key = []
        for param_name, value in hparams.to_list():
            if param_name == PARAM_TTL:
                params_key.append((PARAM_TTL, value))
            elif param_name == PARAM_BRANCH:
                params_key.append((PARAM_BRANCH, value.make_key()))
            elif param_name == PARAM_MADDR:
                if isinstance(value, (ipaddress.IPv4Address, ipaddress.IPv6Address)):
                    app_val = value
                else:
                    app_val = value.lower()
                params_key.append(app_val)
            elif param_name == PARAM_RPORT:
                if isinstance(value, bool):
                    params_key.append(f'{value}')
                else:
                    params_key.append(value)
            else:
                params_key.append((param_name.lower(), value))
        return params_key

    def __eq__(self, other):
        if isinstance(other, ViaHeader):
            return self.sent_protocol == other.sent_protocol and self.sent_by == other.sent_by and \
                   self.make_param_key(self.hparams) == self.make_param_key(other.hparams)
        return NotImplemented

    def assemble(self):
        """Makes a string that represents Via header.

        Returns:
            str: Via header string.
        """
        hparams_str = self.hparams.assemble()
        if hparams_str:
            hparams_str = f';{hparams_str}'
        ret_val = f'{self.sent_protocol} {self.sent_by}{hparams_str}'
        return ret_val

    def __repr__(self):
        return self.assemble()

    def has_rport(self):
        """Is RPORT defined in Via header.

        Returns:
            bool: True if rport defined in Via parameters, False otherwise.
        """
        rport = self.hparams.find(PARAM_RPORT)
        if not isinstance(rport, HParamNotFound):
            return True
        return False

    @property
    def rport(self):
        """RPORT value.

        Returns:
            :obj:ipaddress || str: Received parameter. If no received is specified, None is returned.
        """
        rport = self.hparams.find(PARAM_RPORT)
        if not isinstance(rport, HParamNotFound):
            return rport
        return None

    def build(self, header_name):
        raise NotImplementedError

'''
rport(#via{hparams = HParams}) ->
    case ersip_hparams:find(rport, HParams) of
        {ok, RPortVal} -> {ok, RPortVal};
        not_found      -> undefined
    end.

-spec assemble(via()) -> iolist().
assemble(#via{} = Via) ->
    #via{
       sent_protocol =
           {sent_protocol, Protocol, ProtocolVersion, Transport},
       sent_by    = {sent_by, Host, Port},
       hparams = HParams
      } = Via,
    [Protocol, $/, ProtocolVersion, $/, ersip_transport:assemble_upper(Transport),
     <<" ">>, ersip_host:assemble(Host),
     case Port of
         default_port ->
             [];
         Port ->
             [$:, integer_to_binary(Port)]
     end,
     assemble_params(HParams)
    ].

-spec assemble_bin(via()) -> binary().
assemble_bin(#via{} = Via) ->
    iolist_to_binary(assemble(Via)).
    
    
make_key(#via{hparams = HParams} = Via) ->
    {sent_protocol_make_key(sent_protocol(Via)),
     sent_by_make_key(sent_by(Via)),
     via_params_make_key(HParams)}.
     
   sent_protocol_make_key(Protocol) ->
    Protocol.

-spec sent_by_make_key(sent_by()) -> sent_by().
sent_by_make_key({sent_by, Host, Port}) ->
    {sent_by, ersip_host:make_key(Host), Port}.

-spec via_params_make_key(ersip_hparams:hparams()) -> via_params_key().
via_params_make_key(Params) ->
    L = ersip_hparams:to_list(Params),
    LKeys = lists:map(fun({Key, Value}) ->
                              via_param_make_key(Key, Value)
                      end,
                      L),
    maps:from_list(LKeys).

-spec via_param_make_key(Key, Value) -> {NewKey, NewValue} when
      Key    :: known_via_params() | binary(),
      Value  :: term(),
      NewKey :: known_via_params() | binary(),
      NewValue :: term().
via_param_make_key(ttl, V) ->
    {ttl, V};
via_param_make_key(branch, B) ->
    {branch, ersip_branch:make_key(B)};
via_param_make_key(maddr, Maddr) ->
    {maddr, ersip_host:make_key(Maddr)};
via_param_make_key(received, R) ->
    {received, R};
via_param_make_key(rport, R) ->
    {rport, R};
via_param_make_key(OtherKey, OtherValue) when is_binary(OtherKey) ->
    {ersip_bin:to_lower(OtherKey), OtherValue}.

topmost_via(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_via};
        [TopVia | _]  ->
            parse_via(iolist_to_binary(TopVia))
    end.

-spec do_parse_via_params(binary()) -> ersip_parser_aux:parse_result(ersip_hparams:hparams()).
do_parse_via_params(Bin) ->
    case ersip_hparams:parse_raw(Bin) of
        {ok, HParams0, Rest} ->
            case ersip_hparams:parse_known(fun parse_known/2, HParams0) of
                {ok, HParams} ->
                    {ok, HParams, Rest};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.


parse_sent_by(Binary) ->
    case binary:match(Binary, <<";">>) of
        {Pos, 1} ->
            HostPort = binary:part(Binary, {0, Pos}),
            Rest = binary:part(Binary, Pos, byte_size(Binary) - Pos),
            parse_sent_by_host_port(ersip_bin:trim_lws(HostPort), host, #{rest => Rest});
        nomatch ->
            parse_sent_by_host_port(ersip_bin:trim_lws(Binary), host, #{rest => <<>>})
    end.

parse_sent_by_host_port(<<$[, _/binary>> = IPv6RefPort, host, Acc) ->
    case binary:match(IPv6RefPort, <<"]">>) of
        nomatch ->
            {error, {invalid_ipv6_reference, IPv6RefPort}};
        {Pos, 1} ->
            HostBin = binary:part(IPv6RefPort, {0, Pos+1}),
            Rest = binary:part(IPv6RefPort, {Pos+1, byte_size(IPv6RefPort)-Pos-1}),
            case ersip_host:parse(HostBin) of
                {ok, Host, <<>>} ->
                    parse_sent_by_host_port(Rest, port, Acc#{host => Host});
                {ok, _Host, _Rest} ->
                    {error, {invalid_ipv6_reference, IPv6RefPort}};
                {error, _} = Err ->
                    Err
            end
    end;
parse_sent_by_host_port(Binary, host, Acc) ->
    [HostBin | MayBePort] = binary:split(Binary, <<":">>),
    case ersip_host:parse(HostBin) of
        {ok, Host, <<>>} ->
            parse_sent_by_host_port(MayBePort, port, Acc#{host => Host});
        {ok, _Host, _Rest} ->
            {error, {invalid_host, HostBin}};
        {error, _} = Err ->
            Err
    end;
parse_sent_by_host_port([], port, Acc) ->
    parse_sent_by_host_port(<<>>, result, Acc);
parse_sent_by_host_port(<<>>, port, Acc) ->
    parse_sent_by_host_port(<<>>, result, Acc);
parse_sent_by_host_port(<<":", Rest/binary>>, port, Acc) ->
    parse_sent_by_host_port([Rest], port, Acc);
parse_sent_by_host_port([Bin], port, Acc) when is_binary(Bin) ->
    case ersip_transport:parse_port_number(ersip_bin:trim_lws(Bin)) of
        {ok, PortNumber, <<>>} ->
            parse_sent_by_host_port(<<>>, result, Acc#{port => PortNumber});
        _ ->
            {error, {invalid_port, Bin}}
    end;
parse_sent_by_host_port(_, result, #{rest := Rest, host := Host} = Acc) ->
    Port = maps:get(port, Acc, default_port),
    {ok, {sent_by, Host, Port}, Rest}.



-spec parse_transport(binary()) -> ersip_parser_aux:parse_result(ersip_transport:transport()).
parse_transport(Binary) ->
    case ersip_parser_aux:parse_token(Binary) of
        {ok, Transp, Rest} ->
            {ok, T} = ersip_transport:parse(Transp),
            {ok, T, Rest};
        {error, _} = Error ->
            Error
    end.

parse_sent_protocol(Binary) ->
    Parsers = [fun ersip_parser_aux:parse_token/1,
               fun ersip_parser_aux:parse_slash/1,
               fun ersip_parser_aux:parse_token/1,
               fun ersip_parser_aux:parse_slash/1,
               fun parse_transport/1
              ],
    case ersip_parser_aux:parse_all(Binary, Parsers) of
        {ok, [ProtocolName, _, ProtocolVersion, _, Transport], Rest} ->
            {ok, {sent_protocol, ProtocolName, ProtocolVersion, Transport}, Rest};
        {error, _} = Error ->
            Error
    end.

-spec parse_via(binary()) -> {ok,via()} | {error, term()}.
parse_via(ViaBinary) ->
    Parsers = [fun parse_sent_protocol/1,
               fun ersip_parser_aux:parse_lws/1,
               fun parse_sent_by/1,
               fun ersip_parser_aux:trim_lws/1,
               fun parse_via_params/1],
    case ersip_parser_aux:parse_all(ViaBinary, Parsers) of
        {ok, [SentProtocol, _, SentBy, _, HParams], _} ->
            Via = #via{sent_protocol = SentProtocol,
                       sent_by       = SentBy,
                       hparams       = HParams},
            {ok, Via};
        {error, _} = Error ->
            Error
    end.

%% sent-protocol     =  protocol-name SLASH protocol-version
%%                      SLASH transport
%% protocol-name     =  "SIP" / token
%% protocol-version  =  token
%% transport         =  "UDP" / "TCP" / "TLS" / "SCTP"
%%                      / other-transport
-spec parse_sent_protocol(binary()) -> ersip_parser_aux:parse_result().


%% sent-by           =  host [COLON port]
%% this implementation is not pure. It uses sent-by context in via
%% header (expects that either EOL or SEMI occur after sent-by).
-spec parse_sent_by(binary()) -> ersip_parser_aux:parse_result().




-spec parse(iolist()) -> {ok, via()} | {error, term()}.
parse(IOList) ->
    parse_via(iolist_to_binary(IOList)).


set_param(received, Value, Via) when is_binary(Value) ->
    case ersip_host:parse(Value) of
        {ok, Host, <<>>} ->
            set_param(received, Host, Via);
        {ok, _, _} ->
            error({error, {invalid_host, Value}});
        {error, _} = Error ->
            error(Error)
    end;
set_param(received, {Type, _} = Value , #via{hparams = HP} = Via)
  when Type =:= ipv4 orelse Type =:= ipv6 ->
    HPNew = ersip_hparams:set(received, Value, <<"received">>, assemble_param_value(received, Value), HP),
    Via#via{hparams = HPNew};
set_param(received, Value, _) ->
    error({error, {bad_received_via_param, Value}});
set_param(rport, Value, #via{hparams = HP} = Via)
  when is_integer(Value) andalso Value >= 1 andalso Value =< 65535 orelse Value == true ->
    HPNew = ersip_hparams:set(rport, Value, <<"rport">>, assemble_param_value(rport, Value), HP),
    Via#via{hparams = HPNew};
set_param(rport, Value, _) ->
    error({error, {bad_rport_via_param, Value}}).


-spec assemble_param_value(known_via_params(), term()) -> iolist().
assemble_param_value(received, Value) ->
    ersip_host:assemble(Value);
assemble_param_value(rport, true) ->
    <<>>;
assemble_param_value(rport, Value) ->
    integer_to_binary(Value);
assemble_param_value(ttl, Value) ->
    integer_to_binary(Value);
assemble_param_value(maddr, Value) ->
    ersip_host:assemble(Value);
assemble_param_value(branch, Value) ->
    ersip_branch:assemble(Value).
%%%===================================================================
%%% Types
%%%===================================================================

-record(via, {sent_protocol  :: sent_protocol(),
              sent_by        :: internal_sent_by(),
              hparams        :: ersip_hparams:hparams()
             }).
-type via()           :: #via{}.
-type sent_protocol() :: {sent_protocol, Protocol :: binary(), ProtocolVersion :: binary(), ersip_transport:transport()}.
-type sent_by()       :: {sent_by, ersip_host:host(), Port :: ersip_transport:port_number()}.
-type internal_sent_by() :: {sent_by, ersip_host:host(), Port :: ersip_transport:port_number() | default_port}.
-type known_via_params() :: branch
                          | maddr
                          | received
                          | ttl
                          | rport.
-type rport_value() :: ersip_transport:port_number() | true.
-type via_key() :: {sent_protocol(), sent_by(), via_params_key()}.
-type via_params_key()    :: #{branch   => ersip_branch:branch(),
                               maddr    => ersip_host:host(),
                               received => ersip_host:host(),
                               ttl      => non_neg_integer(),
                               rport    => ersip_transport:port_number() | true,
                               binary() => binary()
                              }.

-spec topmost_via(ersip_hdr:header()) -> Result when
      Result :: {ok, via()}
              | {error, Error},
      Error  :: no_via.

-spec sent_protocol(via()) -> sent_protocol().
sent_protocol(#via{sent_protocol = Sp}) ->
    Sp.

-spec raw_param(ParamName :: binary(), via()) -> {ok, ParamValue :: binary()} | not_found.
raw_param(ParamName, #via{hparams = HParams}) when is_binary(ParamName) ->
    ersip_hparams:find_raw(ParamName, HParams).

-spec set_param(ParamName, Value, via()) -> via() when
      ParamName :: known_via_params() | binary(),
      Value     :: binary()
                 | ersip_host:host()
                 | ersip_transport:port_number().


-spec sent_by(via()) -> sent_by().
sent_by(#via{sent_by = {sent_by,Host,default_port}} = Via) ->
    {sent_protocol, _, _, Transport} = sent_protocol(Via),
    {sent_by, Host, ersip_transport:default_port(Transport)};
sent_by(#via{sent_by = SentBy}) ->
    SentBy.

%% @doc Make comparable sent_by (adjusted to be comparable as erlang
%% terms).
-spec sent_by_key(via()) -> sent_by().
sent_by_key(#via{} = Via) ->
    sent_by_make_key(sent_by(Via)).



-spec has_rport(via()) -> boolean().
has_rport(#via{hparams = HParams}) ->
    case ersip_hparams:find(rport, HParams) of
        {ok, _} ->   true;
        not_found -> false
    end.

-spec rport(via()) -> {ok, rport_value()} | undefined.
rport(#via{hparams = HParams}) ->
    case ersip_hparams:find(rport, HParams) of
        {ok, RPortVal} -> {ok, RPortVal};
        not_found      -> undefined
    end.


-spec maddr(via()) -> {ok, ersip_host:host()} | undefined.
maddr(#via{hparams = HParams}) ->
    case ersip_hparams:find(maddr, HParams) of
        {ok, Host} -> {ok, Host};
        not_found  -> undefined
    end.

-spec ttl(via()) -> {ok, non_neg_integer()} | undefined.
ttl(#via{hparams = HParams}) ->
    case ersip_hparams:find(ttl, HParams) of
        {ok, TTL} -> {ok, TTL};
        not_found -> undefined
    end.

-spec make_key(via()) -> via_key().






%%%===================================================================
%%% Internal implementation
%%%===================================================================



%% via-params        =  via-ttl / via-maddr
%%                      / via-received / via-branch
%%                      / via-extension
-spec parse_known(Key, Value) -> Result when
      Key    :: binary(),
      Value  :: binary(),
      Result :: {ok, {ResultKey, ResultVal}}
              | {error, term()},
      ResultKey :: known_via_params() | binary(),
      ResultVal :: non_neg_integer()
                 | ersip_host:host()
                 | binary().


-spec sent_protocol_make_key(sent_protocol()) -> sent_protocol().


-spec assemble_params(ersip_hparams:hparams()) -> [iolist()].
assemble_params(HParams) ->
    HParamsIO0 = ersip_hparams:assemble(HParams),
    case ersip_iolist:is_empty(HParamsIO0) of
        true -> [];
        false -> [$; | HParamsIO0]
    end.



'''