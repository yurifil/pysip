from pysip import PySIPException
from pysip.message.parser_aux import check_token

UDP = 'udp'
TCP = 'tcp'
TLS = 'tls'
WSS = 'wss'
WS = 'ws'
KNOWN = 'transport'
UNKNOWN = 'other_transport'


class TransportError(PySIPException):
    pass


class Transport(object):
    KNOWN_TRANSPORT = (UDP, TCP, TLS, WSS, WS)

    def __init__(self, transport=None):
        self.name = None
        self.known = None
        if transport is not None:
            self.known, self.name = Transport.parse(transport)

    def __eq__(self, other):
        if isinstance(other, Transport):
            return self.known == other.known and self.name == other.name
        return NotImplemented

    def assemble(self):
        return self.name.upper()

    def __repr__(self):
        return self.assemble()

    @property
    def transport(self):
        return self.known, self.name

    @staticmethod
    def parse(transport):
        """Parses transport string.

        Args:
            transport (str): string containing transport info.

        Returns:
            'transport', str.lower(): if transport is in (UDP, TCP, TLS, WSS, WS)
            'other_transport', str: otherwise.
        """
        if isinstance(transport, str):
            if transport.lower() in Transport.KNOWN_TRANSPORT:
                return KNOWN, transport.lower()
            else:
                if check_token(transport):
                    return UNKNOWN, transport
            raise TransportError(f'Cannot parse transport {transport}: invalid transport')

    @staticmethod
    def default_port(transport):
        """Maps transport on default ports for specified transport.

        Args:
            transport (obj:Transport): Transport object to be analyzed.

        Returns:
            int: default port for specified transport if transport is in (TCP, UDP, TLS, WS, WSS)
            None: if transport is unknown
        """
        if transport.known == KNOWN:
            if transport.name == TCP or transport.name == UDP:
                return 5060
            if transport.name == TLS:
                return 6061
            if transport.name == WS:
                return 80
            if transport.name == WSS:
                return 443
        elif transport.known == UNKNOWN:
            return None

    @staticmethod
    def is_tls(transport):
        if transport.known == KNOWN:
            if transport.name in (TCP, UDP, WS):
                return False
            elif transport.name in (TLS, WSS):
                return True
        raise TransportError(f'Unknown transport')


'''
-spec parse_bin(binary()) -> transport() | {error, {einval, transport}}.
parse_bin(V) ->
    case ersip_bin:to_lower(V) of
        <<"tcp">> -> {transport, tcp};
        <<"udp">> -> {transport, udp};
        <<"tls">> -> {transport, tls};
        <<"wss">> -> {transport, wss};
        <<"ws">>  -> {transport, ws };
        Bin ->
            case ersip_parser_aux:check_token(Bin) of
                true  -> {other_transport, Bin};
                false -> {error, {einval, transport}}
            end
    end.


parse(V) when is_binary(V) ->
    case parse_bin(V) of
        {error, _} = Error ->
            Error;
        T ->
            {ok, T}
    end;
parse(V) when V =:= tcp; V =:= udp; V =:= tls; V =:= wss; V =:= ws ->
    {ok, {transport, V}};
parse(V) when is_atom(V) ->
    {error, {bad_transport_atom, V}}.
'''