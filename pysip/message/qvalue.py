import re
from pysip import PySIPException


class QValueError(PySIPException):
    pass


class QValue(object):
    ONE_AND_ZEROS_RX = re.compile(r'^1\.0*$')
    ZERO_AND_THREE_SYMBOLS_RX = re.compile(r'^0\.(.{0,3})$')

    def __init__(self, qvalue):
        self._qvalue = self.parse(qvalue)

    def __eq__(self, other):
        if isinstance(other, QValue):
            return self._qvalue == other._qvalue
        return NotImplemented

    def __ge__(self, other):
        if isinstance(other, QValue):
            return self._qvalue >= other._qvalue
        return NotImplemented

    def __gt__(self, other):
        if isinstance(other, QValue):
            return self._qvalue > other._qvalue
        return NotImplemented

    def __le__(self, other):
        if isinstance(other, QValue):
            return self._qvalue <= other._qvalue
        return NotImplemented

    def __lt__(self, other):
        if isinstance(other, QValue):
            return self._qvalue < other._qvalue
        return NotImplemented

    @staticmethod
    def parse(string):
        if string == '0':
            return 0
        elif string == '1':
            return 1000
        elif QValue.ONE_AND_ZEROS_RX.match(string):
            return 1000
        elif QValue.ZERO_AND_THREE_SYMBOLS_RX.match(string):
            other_three_symbols = '{:<03}'.format(QValue.ZERO_AND_THREE_SYMBOLS_RX.match(string).group(1))
            try:
                return int(other_three_symbols)
            except ValueError:
                raise QValueError(f'Cannot parse qvalue {string}')
        else:
            raise QValueError(f'Cannot parse qvalue {string}')

    def set(self, value):
        self.qvalue = value

    @property
    def qvalue(self):
        if self._qvalue == 1000:
            return '1'
        else:
            return '0.{:>03}'.format(self._qvalue)

    @qvalue.setter
    def qvalue(self, value):
        self._qvalue = self.parse(value)

'''
assemble({qvalue, 1000}) ->
    <<"1">>;
assemble({qvalue, Value}) ->
    ValueBin = integer_to_binary(Value),
    case byte_size(ValueBin) of
        1 -> <<"0.00", ValueBin/binary>>;
        2 -> <<"0.0", ValueBin/binary>>;
        3 -> <<"0.", ValueBin/binary>>
    end.

%% qvalue         =  ( "0" [ "." 0*3DIGIT ] )
%%                  / ( "1" [ "." 0*3("0") ] )
-spec parse_impl(binary()) -> parse_result().
parse_impl(<<"0">>) ->
    {ok, {qvalue, 0}};
parse_impl(<<"1">>) ->
    {ok, {qvalue, 1000}};
parse_impl(<<"1.", AllZeroes/binary>> = QVal) ->
    case all_zeroes(AllZeroes) of
        true ->
            {ok, {qvalue, 1000}};
        false ->
            {error, {invalid_qvalue, QVal}}
    end;
parse_impl(<<"0.", Rest/binary>> = QVal) when byte_size(Rest) =< 3 ->
    ValueBin = add_zeroes(Rest),
    try
        {ok, {qvalue, binary_to_integer(ValueBin)}}
    catch
        error:badarg ->
            {error, {invalid_qvalue, QVal}}
    end;
parse_impl(QVal) ->
    {error, {invalid_qvalue, QVal}}.

-spec all_zeroes(binary()) -> boolean().
all_zeroes(<<>>) ->
    true;
all_zeroes(<<"0", Rest/binary>>) ->
    all_zeroes(Rest);
all_zeroes(_) ->
    false.

-spec add_zeroes(binary()) -> binary().
add_zeroes(X) when byte_size(X) == 3 -> X;
add_zeroes(X) when byte_size(X) == 2 -> <<X/binary, "0">>;
add_zeroes(X) when byte_size(X) == 1 -> <<X/binary, "00">>;
add_zeroes(X) when byte_size(X) == 0 -> <<"000">>.

-module(ersip_qvalue).

-export([make/1,
         parse/1,
         assemble/1
        ]).

-export_type([qvalue/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type qvalue() :: {qvalue, 0..1000}.
-type parse_result() :: {ok, qvalue()}
                      | {error, {invalid_qvalue, binary()}}.

%%%===================================================================
%%% API
%%%===================================================================

-spec make(binary()) -> qvalue().
make(Bin) ->
    case parse(Bin) of
        {ok, QValue} ->
            QValue;
        {error, Reason} ->
            error(Reason)
    end.

%% qvalue         =  ( "0" [ "." 0*3DIGIT ] )
%%                  / ( "1" [ "." 0*3("0") ] )
-spec parse(binary()) -> parse_result().
parse(Bin) ->
    parse_impl(Bin).

-spec assemble(qvalue()) -> binary().


%%%===================================================================
%%% Internal implementation
%%%===================================================================


'''
