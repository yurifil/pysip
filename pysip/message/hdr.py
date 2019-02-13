from pysip.binary import to_integer, to_string, to_bytes
from pysip.message.hnames import make_key
from pysip import PySIPException


def may_have_multiple_values(header_key):
    return MAY_HAVE_MULTIPLE_VALUE_MAP.get(header_key, False)


MAY_HAVE_MULTIPLE_VALUE_MAP = {
    make_key('from'): False,  # From
    make_key('to'): False,  # To
    make_key('call-id'): False,  # Call-ad
    make_key('max-forwards'): False,
    make_key('expires'): False,
    make_key('cseq'): False,
    make_key('via'): True,  # Via
    make_key('contact'): False,  # Contact
    make_key('supported'): True,  # Supported
    make_key('unsupported'): True,
    make_key('allow'): True,
    make_key('route'): True,
    make_key('record-route"'): True,
    make_key('require'): True,
    make_key('proxy-require'): True,
    make_key('accept'): True,
    make_key('accept-encoding'): True,
    make_key('accept-language'): True,
    make_key('www-authenticate'): True,  # WWW-Authenticate may have multiple values but they can't be comma separated
    make_key('authorization'): True,  # Authorization may have multiple values but they can't be comma separated
    make_key('proxy-authenticate'): True,  # Proxy-Authenticate may have multiple values but they can't be comma separated
    make_key('proxy-authorization'): True  # Proxy-Authorization may have multiple values but they can't be comma separated
}


CANNOT_BE_COMMA_SEPARATED = (make_key('www-authenticate'), make_key('authorization'), make_key('proxy-authenticate'),
                             make_key('proxy-authorization'))

DO_NOT_USE_COMMA = (make_key('via'), make_key('contact'))


class BaseSipHeader(object):
    @staticmethod
    def parse(header):
        raise NotImplementedError

    def assemble(self):
        raise NotImplementedError

    def build(self, header_name):
        raise NotImplementedError


class HeaderError(PySIPException):
    pass


class Header(object):
    ENCODING = 'utf-8'
    COMMA = ','

    def __init__(self, name):
        self.name = to_string(name, encoding=self.ENCODING)
        self.values = []
        self.key = make_key(self.name)
        self.allow_multiple_values = may_have_multiple_values(self.key)

    def __repr__(self):
        return f'{self.name}={self.values}'

    def __eq__(self, other):
        if isinstance(other, Header) or issubclass(other.__class__, Header):
            return self.key == other.key and self.values == other.values
        return NotImplemented

    def is_empty(self):
        return len(self.values) == 0

    def add_value(self, val):
        if self.allow_multiple_values and self.key not in CANNOT_BE_COMMA_SEPARATED:
            for v in to_string(val).split(self.COMMA):
                self.values.append(to_string(v).strip())
        else:
            self.values.append(to_string(val))

    def add_values(self, val_list):
        for val in val_list:
            self.add_value(val)

    def as_integer(self):
        if len(self.values) > 1:
            raise HeaderError(f'Cannot represent header {self} value as integer: multiple values.')
        elif not self.values:
            raise HeaderError(f'Cannot represent header {self} value as integer: no value.')
        try:
            return to_integer(self.values[0])
        except ValueError as e:
            raise HeaderError(f'Cannot represent header {self} value as integer: {e}.')

    def serialize_to_bytes(self, append_value=None):
        return to_bytes(self.serialize_to_string(append_value=append_value))

    def serialize_to_string(self, append_value=None):
        if len(self.values) < 1 and append_value is None:
            raise HeaderError(f'Cannot serialize header {self}: no values.')
        ret_val = ''
        if self.key not in DO_NOT_USE_COMMA:
            if self.values:
                ret_val = f'{self.name}: {", ".join(self.values)}'
        else:
            if self.values:
                ret_val = '\r\n'.join([f'{self.name}: {val}' for val in self.values])
        if append_value is not None:
            if ret_val:
                ret_val = f'{to_string(append_value)}\r\n{ret_val}'
            else:
                ret_val = to_string(append_value)
        return ret_val

'''
serialize_to_bin(Header) ->
    iolist_to_binary(lists:reverse(ersip_hdr:serialize_rev_iolist(Header, []))).

serialize_rev_iolist(#header{key = Key} = Header, Acc) ->
    case use_comma(Key) of
        true ->
            serialize_rev_iolist_comma_impl(ensure_raw_values(Header), Acc);
        false ->
            serialize_rev_iolist_impl(ensure_raw_values(Header), Acc)
    end.

-spec use_comma(header_key()) -> boolean().
use_comma({hdr_key, <<"v">>}) -> %% Via
    false;
use_comma({hdr_key, <<"m">>}) -> %% Contact
    false;
use_comma(_) ->
    true.

-spec rev_comma_sep_values([value()], value()) -> iolist().
rev_comma_sep_values([LastVal], Acc) ->
    [LastVal | Acc];
rev_comma_sep_values([Val | Rest], Acc) ->
    rev_comma_sep_values(Rest, [<<", ">>, Val | Acc]).

serialize_rev_iolist_comma_impl(#header{values = []}, Acc) ->
    Acc;
serialize_rev_iolist_comma_impl(#header{name = Name, values = Vs}, []) ->
    rev_comma_sep_values(Vs, [<<": ">> , Name]);
serialize_rev_iolist_comma_impl(#header{name = Name, values = Vs}, Acc) ->
    rev_comma_sep_values(Vs, [<<": ">> , Name, <<"\r\n">> | Acc]).
    
serialize_rev_iolist_impl(#header{values = []}, Acc) ->
    Acc;
serialize_rev_iolist_impl(#header{name = Name, values = [V | Rest]} = H, []) ->
    serialize_rev_iolist_impl(H#header{values = Rest},
                              [V, <<": ">>, Name]);
serialize_rev_iolist_impl(#header{name = Name, values = [V | Rest]} = H, Acc) ->
    serialize_rev_iolist_impl(H#header{values = Rest},
                              [V, <<": ">>, Name, <<"\r\n">> | Acc]).
'''