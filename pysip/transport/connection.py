from pysip.transport.parser import Parser, MoreDataRequired
from pysip.message.hdr_via import ViaHeader, SentBy, SentProtocol, PARAM_RECEIVED, PARAM_RPORT
from pysip.message.hnames import VIA_HEADER
from pysip.message.hdr import NoHeaderError
from pysip.source import Source
from pysip.message.message import TYPE_REQUEST, TYPE_RESPONSE
from pysip import PySIPException

from ipaddress import IPv6Address, IPv4Address

PARSER_OPTIONS_KEY = 'parser'
SOURCE_ID_OPTION_KEY = 'source_id'


'''
-spec bad_message(binary() | ersip_msg:message(), Reason :: term()) -> bad_message().
bad_message(Data, Error) ->
    {bad_message, Data, Error}.

-spec new_request(ersip_msg:message()) -> new_request().
new_request(Message) ->
    {new_request, Message}.

-spec new_response(ersip_hdr_via:via(), ersip_msg:message()) -> new_response().
new_response(Via, Message) ->
    {new_response, Via, Message}.

-spec disconnect({error, term()}) -> disconnect().
disconnect(Error) ->
    {disconnect, Error}.
'''


class NewResponse(object):
    effect = 'new_response'

    def __init__(self, via=None, message=None):
        self.via = via
        self.message = message

    def __repr__(self):
        return f'{self.__class__.__name__}(via={self.via}, message={self.message})'


class NewRequest(object):
    effect = 'new_request'

    def __init__(self, message=None):
        self.message = message

    def __repr__(self):
        return f'{self.__class__.__name__}(message={self.message})'


class Disconnect(object):
    effect = 'disconnect'

    def __init__(self, error=None):
        self.error = error

    def __repr__(self):
        return f'{self.__class__.__name__}({self.error})'


def disconnect(error):
    return 'disconnect', error


def new_response(via, message):
    return 'new_response', via, message


def new_request(message):
    return 'new_request', message


class SipConnectionError(PySIPException):
    pass


class SipConnection(object):
    def __init__(self, local_addr=None, local_port=None, remote_addr=None, remote_port=None, transport=None,
                 options=None):
        self.local_addr = local_addr
        self.local_port = local_port
        self.remote_addr = remote_addr
        self.remote_port = remote_port
        self.transport = transport
        if options is None:
            options = dict()
        self.options = options
        self.parser = None
        parser_options = self.options.get(PARSER_OPTIONS_KEY, dict())
        if transport.is_datagram():
            self.parser = Parser.new(parser_options)
        print(f'SipConnection(local_addr={self.local_addr}, local_port={local_port}, remote_addr={self.remote_addr}, '
              f'remote_port={self.remote_port}, transport={self.transport}, options={self.options})')

    def connection_data(self, string):
        """

        Args:
            string (str): connection data to be parsed

        Returns:

        """
        print(f'StatelessProxy.connection_data()')
        if self.parser is None:
            dgram = Parser.new_dgram(string)
            try:
                parsed, data = Parser.parse(dgram)
                if not isinstance(parsed, MoreDataRequired):
                    return self.receive_raw(parsed)
                return self.return_side_effects('sf')
            except Exception as e:
                self.return_side_effects(e)
        else:
            print(f'SipConnection.connection_data(): datagram transport')
            print(f'SipConnection.connection_data(): string {string}')
            self.parser.add(string)
            return self.parse_data([])

    def add_via(self, message, branch):
        via_hdr = message.get(VIA_HEADER)
        via = ViaHeader()
        via.sent_by = SentBy(host=self.local_addr, port=self.local_port)
        via.sent_protocol = SentProtocol(transport=self.transport, version='2.0', name='SIP')
        via.branch = branch
        via_hdr.add_topmost(via)
        message.set_header(via_hdr)

    def take_via(self, message):
        via_hdr = message.get(VIA_HEADER)
        try:
            value = via_hdr.take_topmost()
            try:
                via = ViaHeader.parse(value)
                via_match = self.check_via_match(via)
                if via_match:
                    message.set_header(via_hdr)
                else:
                    raise SipConnectionError(f'Via mismatch: expected {via_match.expected}, got {via_match.actual}')
            except Exception as e:
                raise SipConnectionError(f'Bad via: {e}')
        except NoHeaderError:
            raise SipConnectionError(f'No via.')

    @property
    def source(self):
        source_id = self.options.get(SOURCE_ID_OPTION_KEY)
        return Source(peer=self.remote_addr, transport=self.transport, source_id=source_id)

    def receive_raw(self, msg):
        print(f'StatelessProxy.receive_raw(): {msg.type}')
        if msg.type == TYPE_REQUEST:
            print(f'StatelessProxy.receive_raw(): processing {TYPE_REQUEST}')
            return self.receive_request(msg)
        elif msg.type == TYPE_RESPONSE:
            return self.receive_response(msg)
        else:
            raise ConnectionError(f'Cannot receive message {msg}: unknown type {msg.type}')

    def receive_request(self, msg):
        try:
            print(f'StatelessProxy.receive_request()')
            self.receive_request_process_via(msg)
            msg.source = self.source
            return NewRequest(message=msg)
        except Exception as e:
            self.return_side_effects(e)

    def receive_response(self, msg):
        try:
            via = self.take_via(msg)
            msg.source = self.source
            return NewResponse(via=via, message=msg)
        except Exception as e:
            self.return_side_effects(e)

    def parse_data(self, side_effects):
        print(f'StatelessProxy.parse_data()')
        try:
            msg, data = Parser.parse(self.parser)
            if isinstance(msg, MoreDataRequired):
                return side_effects
            else:
                print(f'StatelessProxy.parse_data(): parsed {msg}')
                result = self.receive_raw(msg)
                print(f'StatelessProxy.parse_data(): receive result {result}')
                side_effects = self.add_side_effects_to_head([result], side_effects)
                print(f'StatelessProxy.parse_data(): side effects {side_effects}')
                return self.parse_data(side_effects)
        except Exception as e:
            return self, side_effects + [Disconnect(e)]

    def receive_request_process_via(self, msg):
        print(f'StatelessProxy.receive_request_process_via()')
        via_hdr = msg.get(VIA_HEADER)
        print(f'StatelessProxy.receive_request_process_via(): via header {via_hdr}')
        via = ViaHeader.topmost_via(via_hdr)
        print(f'StatelessProxy.receive_request_process_via(): topmost via {via}')
        self.maybe_add_received(via)
        print(f'StatelessProxy.receive_request_process_via(): via with received {via}')
        self.maybe_fill_rport(via)
        print(f'StatelessProxy.receive_request_process_via(): via with rport {via}')
        via_hdr.replace_topmost(via.assemble())
        print(f'StatelessProxy.receive_request_process_via(): via with replaced topmost {via_hdr}')
        msg.set_header(via_hdr)

    def maybe_add_received(self, via):
        print(f'StatelessProxy.maybe_add_received({via})')
        if isinstance(via.sent_by.host, IPv6Address) or isinstance(via.sent_by.host, IPv4Address) and \
                via.sent_by.host != self.remote_addr:
            print(f'StatelessProxy.maybe_add_received(): host is IP setting received param to {self.remote_addr}')
            via.set_param(PARAM_RECEIVED, self.remote_addr)
        if isinstance(via.sent_by.host, str):
            print(f'StatelessProxy.maybe_add_received(): host is hostname setting received param to {self.remote_addr}')
            via.set_param(PARAM_RECEIVED, self.remote_addr)

    def maybe_fill_rport(self, via):
        if via.has_rport():
            via.set_param(PARAM_RPORT, self.remote_port)
            via.set_param(PARAM_RECEIVED, self.remote_addr)

    def return_side_effects(self, side_effect):
        return [side_effect]

    def add_side_effects_to_head(self, side_effect_list, side_effect):
        print(f'StatelessProxy.add_side_effects_to_head({side_effect_list}, {side_effect})')
        return [side_effect] + side_effect_list

    def check_via_match(self, via):
        if self.check_via_match_address(via) and self.check_via_match_transport(via):
            return ViaMatchResult(result=ViaMatchResult.MATCH)
        else:
            expected = ViaHeader()
            expected.sent_by = SentBy(host=self.local_addr, port=self.local_port)
            expected.sent_protocol = SentProtocol(transport=self.transport, version='2.0', name='SIP')
            expected_str = expected.assemble()
            actual_str = via.assemble()
            return ViaMatchResult(result=ViaMatchResult.MISMATCH, expected=expected_str, actual=actual_str)

    def check_via_match_address(self, via):
        if via.sent_by == SentBy(host=self.local_addr, port=self.local_port):
            return True
        return False

    def check_via_match_transport(self, via):
        if via.sent_protocol == SentProtocol(name='SIP', version='2.0', transport=self.transport):
            return True
        return False


class ViaMatchResult(object):
    MATCH = 'match'
    MISMATCH = 'mismatch'

    def __init__(self, result, expected=None, actual=None):
        self.result = result
        self.expected = expected
        self.actual = actual

    def __bool__(self):
        if self.result == ViaMatchResult.MISMATCH:
            return False
'''
-spec check_via_match(ersip_hdr_via:via(), sip_conn()) -> Result when
      Result :: match
              | {mismatch, Expected :: binary(), Got :: binary()}.
check_via_match(Via, #sip_conn{local_addr = {LocalAddr, LocalPort}, transport = SIPTransport} = SipConn) ->
    Match = check_via_match_address(Via, SipConn)
        andalso check_via_match_transport(Via, SipConn),
    case Match of
        true ->
            match;
        false ->
            Expected = ersip_hdr_via:new(LocalAddr, LocalPort, SIPTransport),
            ExpectedBin = iolist_to_binary(ersip_hdr_via:assemble(Expected)),
            GotBin = iolist_to_binary(ersip_hdr_via:assemble(Via)),
            {mismatch, ExpectedBin, GotBin}
    end.

-spec check_via_match_address(ersip_hdr_via:via(), sip_conn()) -> boolean().
check_via_match_address(Via, #sip_conn{local_addr = {LocalAddr, LocalPort}}) ->
    case ersip_hdr_via:sent_by(Via) of
        {sent_by, LocalAddr, LocalPort} ->
            true;
        _ ->
            false
    end.

-spec check_via_match_transport(ersip_hdr_via:via(), sip_conn()) -> boolean().
check_via_match_transport(Via, #sip_conn{transport = SIPTransport}) ->
    case ersip_hdr_via:sent_protocol(Via) of
        {sent_protocol, <<"SIP">>, <<"2.0">>, SIPTransport} ->
            true;
        _ ->
            false
    end.


-spec return_se(ersip_conn_se:side_effect(), sip_conn()) -> result().
return_se(SideEffect, SipConn) ->
    {SipConn, [SideEffect]}.

-spec add_side_effects_to_head(result(), [ersip_conn_se:side_effect()]) -> result().
add_side_effects_to_head({Conn, SideEffect}, SE) ->
    {Conn, SE ++ SideEffect}.


%% @doc
%% When the server transport receives a request over any transport, it
%% MUST examine the value of the "sent-by" parameter in the top Via
%% header field value.  If the host portion of the "sent-by" parameter
%% contains a domain name, or if it contains an IP address that
%% differs from the packet source address, the server MUST add a
%% "received" parameter to that Via header field value.  This
%% parameter MUST contain the source address from which the packet was
%% received.
-spec maybe_add_received(ersip_hdr_via:via(), sip_conn()) -> ersip_hdr_via:via().
maybe_add_received(Via, #sip_conn{} = Conn) ->
    RemoteIP = remote_ip(Conn),
    case ersip_hdr_via:sent_by(Via) of
        {sent_by, {hostname, _}, _} ->
            ersip_hdr_via:set_param(received, RemoteIP, Via);
        {sent_by, IP, _} when IP =/= RemoteIP ->
            ersip_hdr_via:set_param(received, RemoteIP, Via);
        _ ->
            Via
    end.

-spec maybe_fill_rport(ersip_hdr_via:via(), sip_conn()) -> ersip_hdr_via:via().
maybe_fill_rport(Via, #sip_conn{} = Conn) ->
    case ersip_hdr_via:has_rport(Via) of
        true ->
            %% RFC 3581:
            %% When a server compliant to this specification (which can be a proxy
            %% or UAS) receives a request, it examines the topmost Via header field
            %% value.  If this Via header field value contains an "rport" parameter
            %% with no value, it MUST set the value of the parameter to the source
            %% port of the request.  This is analogous to the way in which a server
            %% will insert the "received" parameter into the topmost Via header
            %% field value.  In fact, the server MUST insert a "received" parameter
            %% containing the source IP address that the request came from, even if
            %% it is identical to the value of the "sent-by" component.  Note that
            %% this processing takes place independent of the transport protocol.
            Via1 = ersip_hdr_via:set_param(rport, remote_port(Conn), Via),
            Via2 = ersip_hdr_via:set_param(received, remote_ip(Conn), Via1),
            Via2;
        false ->
            Via
    end.





-spec receive_request(ersip_msg:message(), sip_conn()) -> result().
receive_request(Msg, Conn) ->
    case receive_request_process_via(Msg, Conn) of
        {ok, NewMsg} ->
            NewMsgWithSrc = ersip_msg:set_source(source(Conn), NewMsg),
            {Conn, [ersip_conn_se:new_request(NewMsgWithSrc)]};
        {error, _} = Error ->
            return_se(ersip_conn_se:bad_message(Msg, Error), Conn)
    end.


    
    

-spec receive_raw(ersip_msg:message(), sip_conn()) -> result().
receive_raw(Msg, #sip_conn{} = Conn) ->
    case ersip_msg:get(type, Msg) of
        request ->
            receive_request(Msg, Conn);
        response ->
            receive_response(Msg, Conn)
    end.


-spec source(sip_conn()) -> ersip_source:source().
source(#sip_conn{remote_addr = Peer, transport = T, options = Opts}) ->
    SourceId = maps:get(source_id, Opts, undefined),
    ersip_source:new(Peer, T, SourceId).
    



-spec add_via(ersip_msg:message(), ersip_branch:branch(), sip_conn()) -> ersip_msg:message().
add_via(Msg, Branch, #sip_conn{local_addr = {LocalAddr, LocalPort}, transport = SIPTransport}) ->
    ViaH = ersip_msg:get(<<"Via">>, Msg),
    Via = ersip_hdr_via:new(LocalAddr, LocalPort, SIPTransport, Branch),
    ViaH1 = ersip_hdr:add_topmost(ersip_hdr_via:assemble(Via), ViaH),
    ersip_msg:set_header(ViaH1, Msg).


%%%===================================================================
%%% Types
%%%===================================================================

-type result() :: {sip_conn(), [ersip_conn_se:side_effect()]}.
-record(sip_conn, {
          local_addr  :: {ersip_host:host(), inet:port_number()},
          remote_addr :: {ersip_host:host(), inet:port_number()},
          transport   :: ersip_transport:transport(),
          options     :: options(),
          parser      :: ersip_parser:data() | undefined
         }).
-type sip_conn() :: #sip_conn{}.
-type options()  :: map().
-type maybe_message() :: {ok, ersip_msg:message()}
                       | {error, term()}.
-type maybe_via() :: {ok, ersip_hdr_via:via()}
                   | {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================



-spec conn_data(binary(), sip_conn()) -> result().
conn_data(Binary, #sip_conn{parser = undefined} = Conn) ->
    %% Datagram transport
    Parser = ersip_parser:new_dgram(Binary),
    case ersip_parser:parse(Parser) of
        {{ok, Msg}, _} ->
            receive_raw(Msg, Conn);
        {more_data, _} ->
            return_se(ersip_conn_se:bad_message(Binary, truncated), Conn);
        {{error, _} = Error, _} ->
            return_se(ersip_conn_se:bad_message(Binary, Error), Conn)
    end;
conn_data(Binary, #sip_conn{parser = Parser} = Conn) ->
    %% Stream transport
    NewParser = ersip_parser:add_binary(Binary, Parser),
    parse_data({save_parser(NewParser, Conn), []}).





%%%===================================================================
%%% Internal Implementation
%%%===================================================================



-spec remote_ip(sip_conn()) -> ersip_host:host().
remote_ip(#sip_conn{remote_addr = {RemoteIP, _}}) ->
    RemoteIP.

-spec remote_port(sip_conn()) -> inet:port_number().
remote_port(#sip_conn{remote_addr = {_, RemotePort}}) ->
    RemotePort.

-spec save_parser(ersip_parser:data(), sip_conn()) -> sip_conn().
save_parser(Parser, SipConn) ->
    SipConn#sip_conn{parser = Parser}.















'''