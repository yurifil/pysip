from pysip import PySIPException
from pysip.message.method_unicode import Method
from pysip.message.hdr import Header, BaseSipHeader
import re


class CSeqHeaderError(PySIPException):
    pass


class CSeqHeader(BaseSipHeader):
    CSEQ_RX = re.compile(r'\s*(\d+)\s+(.*)')

    def __init__(self, method=None, number=1, header=None):
        self._number = None
        if method is not None and header is not None:
            raise CSeqHeaderError(f'Cannot initialize CSeqHeader object: you should specify method or header, not both')
        self.method = method
        self.number = number
        if header is not None:
            self.parse_inner_impl(header)

    @property
    def number(self):
        return self._number

    @number.setter
    def number(self, number):
        if isinstance(number, int):
            int_number = number
        elif isinstance(number, str):
            try:
                int_number = int(number)
            except Exception as e:
                raise CSeqHeaderError(f'Cannot set number to {number}: {e}')
        else:
            raise CSeqHeaderError(f'Cannot set number to {number} (type {number}): should be int or str')
        if int_number < 0:
            raise CSeqHeaderError(f'Cannot set number to {number}: should be non-negative integer')
        self._number = int_number

    @staticmethod
    def parse_cseq(cseq):
        match_res = CSeqHeader.CSEQ_RX.match(cseq)
        if not match_res:
            raise CSeqHeaderError(f'Cannot parse {cseq}: invalid cseq')
        return int(match_res.group(1)), Method(match_res.group(2))

    @staticmethod
    def parse_hdr(header):
        if header.values:
            return CSeqHeader.parse_cseq(''.join(header.values))
        raise CSeqHeaderError(f'Cannot parse header {header}: no cseq.')

    @staticmethod
    def parse(header):
        cseq = CSeqHeader()
        if isinstance(header, Header):
            cseq._number, cseq.method = CSeqHeader.parse_hdr(header)
        elif isinstance(header, str):
            cseq._number, cseq.method = CSeqHeader.parse_cseq(header)
        return cseq

    def parse_inner_impl(self, header):
        if isinstance(header, Header):
            self._number, self.method = self.parse_hdr(header)
        elif isinstance(header, str):
            self._number, self.method = self.parse_cseq(header)

    def assemble(self):
        return f'{self.number} {self.method}'

    def build(self, header_name='CSeq'):
        hdr = Header(header_name)
        hdr.add_value(self.assemble())
        return hdr


'''
-spec parse(ersip_hdr:header()) -> Result when
      Result :: {ok, cseq()}
              | {error, Error},
      Error :: no_cseq
             | multiple_cseqs
             | {invalid_cseq, binary()}.
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_cseq};
        [CSeqIOList]  ->
            parse_cseq(iolist_to_binary(CSeqIOList))
    end.

parse_cseq(Binary) ->
    Parsers = [fun ersip_parser_aux:parse_non_neg_int/1,
               fun ersip_parser_aux:parse_lws/1,
               fun ersip_parser_aux:parse_token/1
              ],
    case ersip_parser_aux:parse_all(Binary, Parsers) of
        {ok, [Number, _, Method], <<>>} ->
            {ok, make(ersip_method:make(Method), Number)};
        {ok, _, _} ->
            {error, {invalid_cseq, Binary}};
        {error, _} = Error ->
            Error
    end.


%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP CSeq header
%%

-module(ersip_hdr_cseq).

-export([new/1,
         make/1,
         make/2,
         make_key/1,
         number/1,
         set_number/2,
         method/1,
         set_method/2,
         parse/1,
         build/2,
         assemble/1
        ]).
-export_type([cseq/0,
              cseq_num/0
             ]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(cseq, {method :: ersip_method:method(),
               number :: cseq_num()
              }).
-type cseq_num() :: non_neg_integer().
-type cseq() :: #cseq{}.


%%%===================================================================
%%% API
%%%===================================================================

-spec new(ersip_method:method()) -> cseq().
new(Method) ->
    %% The sequence number value MUST be expressible as a 32-bit
    %% unsigned integer and MUST be less than 2**31.  As long as it
    %% follows the above guidelines, a client may use any mechanism it
    %% would like to select CSeq header field values.
    #cseq{method = Method, number = 1}.

-spec make(ersip_hdr:header()) -> cseq().
make(Header) ->
    case parse(Header) of
        {ok, CSeq} ->
            CSeq;
        Error ->
            error(Error)
    end.

-spec make(ersip_method:method(), cseq_num()) -> cseq().
make(Method, Number) ->
    #cseq{method = Method,
          number = Number}.

-spec make_key(cseq()) -> cseq().
make_key(CSeq) ->
    CSeq.

-spec number(cseq()) -> cseq_num().
number(#cseq{number = N}) ->
    N.

-spec set_number(cseq_num(), cseq()) -> cseq().
set_number(N, #cseq{} = CSeq) ->
    CSeq#cseq{number = N}.

-spec method(cseq()) -> ersip_method:method().
method(#cseq{method = M}) ->
    M.

-spec set_method(ersip_method:method(), cseq()) -> cseq().
set_method(Method, #cseq{} = CSeq) ->
    CSeq#cseq{method = Method}.

-spec build(HeaderName :: binary(), cseq()) -> ersip_hdr:header().
build(HdrName, #cseq{} = CSeq) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(CSeq), Hdr).

-spec assemble(cseq()) -> [binary(), ... ].
assemble(#cseq{method = Method, number = Num}) ->
    [integer_to_binary(Num),
     <<" ">>,
     ersip_method:to_binary(Method)
    ].

%%%===================================================================
%%% Internal implementation
%%%===================================================================


'''