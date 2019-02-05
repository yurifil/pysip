from pysip import PySIPException
from pysip.message.hdr import Header


class ExpiresHeaderError(PySIPException):
    pass


class ExpiresHeader(object):
    MAX_EXPIRES = 4294967295

    def __init__(self, expires=None):
        self._expires = expires
        if expires is not None:
            self._expires = ExpiresHeader.parse(expires)

    def __eq__(self, other):
        if isinstance(other, ExpiresHeader):
            return self._expires == other._expires
        return NotImplemented

    @property
    def expires(self):
        return self._expires

    @expires.setter
    def expires(self, expires):
        self._expires = self.parse(expires)

    @staticmethod
    def parse(expires):
        if isinstance(expires, int):
            int_expires = expires
        elif isinstance(expires, str):
            try:
                int_expires = int(expires)
            except Exception as e:
                raise ExpiresHeaderError(f'Cannot parse expires {expires}: could not convert to int ({e})')
        elif isinstance(expires, Header):
            return ExpiresHeader.parse_hdr(header=expires)
        else:
            raise ExpiresHeaderError(f'Cannot parse expires {expires}: should be int or str not {type(expires)}')
        if int_expires < 0 or int_expires > ExpiresHeader.MAX_EXPIRES:
            raise ExpiresHeaderError(f'Cannot parse expires {expires}: should be non-negative int < '
                                     f'{ExpiresHeader.MAX_EXPIRES}')
        return int_expires

    @staticmethod
    def parse_hdr(header):
        return ExpiresHeader.parse(''.join(header.values))

'''
%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Expires and Min-Expires headers
%%

-module(ersip_hdr_expires).

-export([make/1,
         parse/1,
         build/2
        ]).

-export_type([expires/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type expires() :: {expires, non_neg_integer()}.
-type parse_result(ErrorType) :: {ok, expires()}
                               | {error, {invalid_expires, ErrorType}}.
-type parse_errors() :: empty_field
                      | binary().

%%%===================================================================
%%% API
%%%===================================================================

-spec make(binary()) -> expires().
make(Bin) when is_binary(Bin) ->
    case parse_expires(Bin) of
        {ok, Expires} ->
            Expires;
        {error, Reason} ->
            error(Reason)
    end.

-spec parse(ersip_hdr:header()) -> parse_result(parse_errors()).
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, {invalid_expires, empty_field}};
        [ExpiresIOList]  ->
            parse_expires(iolist_to_binary(ExpiresIOList))
    end.

-spec build(HdrName, expires()) -> ersip_hdr:header() when
      HdrName :: binary().
build(HdrName, {expires, V}) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([integer_to_binary(V)], Hdr).


%%%===================================================================
%%% Internal implementation
%%%===================================================================

%% The value of this field is an integral number of seconds (in
%% decimal) between 0 and (2**32)-1, measured from the receipt of the
%% request.
-spec parse_expires(binary()) -> parse_result(binary()).
parse_expires(Binary) ->
    case ersip_parser_aux:parse_non_neg_int(Binary) of
        {ok, Int, <<>>} when Int =< 16#FFFFFFFF ->
            {ok, {expires, Int}};
        _ ->
            {error, {invalid_expires, Binary}}
    end.

'''