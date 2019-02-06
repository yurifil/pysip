from pysip import PySIPException
from pysip.message.hdr import Header
from pysip.message.parser_aux import check_token


class OptTagListHeaderError(PySIPException):
    pass


class OptTagListHeader(object):
    def __init__(self, opttaglist=None):
        self.opt_tag_set = None
        if opttaglist is not None:
            self.opt_tag_set = self.parse(opttaglist)

    def __eq__(self, other):
        if isinstance(other, OptTagListHeader):
            return self.opt_tag_set == other.opt_tag_set
        return NotImplemented

    @staticmethod
    def parse(opttaglist):
        if isinstance(opttaglist, Header):
            if len(opttaglist.values) == 0:
                raise OptTagListHeaderError(f'Cannot parse opptaglist {opttaglist}: no values')
            else:
                ret_opttaglist = set()
                for val in opttaglist.values:
                    if not check_token(val):
                        raise OptTagListHeaderError(f'Cannot parse opptaglist {opttaglist}: value {val} is not token')
                    ret_opttaglist.add(val)
                return ret_opttaglist
        else:
            raise OptTagListHeaderError(f'Cannot parse opptaglist {opttaglist}: should be type Header not '
                                        f'{type(opttaglist)}')

    def intersect(self, other):
        if isinstance(other, OptTagListHeader):
            return self.opt_tag_set.intersection(other.opt_tag_set)
        return NotImplemented

    def substract(self, other):
        if isinstance(other, OptTagListHeader):
            return self.opt_tag_set.difference(other.opt_tag_set)
        return NotImplemented

    def assemble(self):
        return ', '.join(self.opt_tag_set)
'''
intersect({option_tag_list, S1}, {option_tag_list, S2}) ->
    {option_tag_list, gb_sets:intersection(S1, S2)}.

-spec subtract(option_tag_list(), option_tag_list()) -> option_tag_list().
subtract({option_tag_list, S1}, {option_tag_list, S2}) ->
    {option_tag_list, gb_sets:subtract(S1, S2)}.

assemble({option_tag_list, _} = OptionTagList) ->
    ersip_iolist:join(<<", ">>,
                      [ersip_option_tag:to_binary(OptionTag)
                       || OptionTag <- to_list(OptionTagList)
                      ]).

%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP option-tag list headers:
%% Supported, Unsupported, Require, Proxy-Require
%%

-module(ersip_hdr_opttag_list).

-export([from_list/1,
         to_list/1,
         intersect/2,
         subtract/2,
         parse/1,
         build/2,
         assemble/1
        ]).
-export_type([option_tag_list/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type option_tag_list() :: {option_tag_list, gb_sets:set(ersip_option_tag:option_tag())}.

%%%===================================================================
%%% API
%%%===================================================================

-spec from_list([ersip_option_tag:option_tag()]) -> option_tag_list().
from_list(OptionTagList) ->
    {option_tag_list, gb_sets:from_list(OptionTagList)}.

-spec to_list(option_tag_list()) -> [ersip_option_tag:option_tag()].
to_list({option_tag_list, OptionTagSet}) ->
    gb_sets:to_list(OptionTagSet).

-spec intersect(option_tag_list(), option_tag_list()) -> option_tag_list().
intersect({option_tag_list, S1}, {option_tag_list, S2}) ->
    {option_tag_list, gb_sets:intersection(S1, S2)}.

-spec subtract(option_tag_list(), option_tag_list()) -> option_tag_list().
subtract({option_tag_list, S1}, {option_tag_list, S2}) ->
    {option_tag_list, gb_sets:subtract(S1, S2)}.

-spec parse(ersip_hdr:header()) -> Result when
      Result :: {ok, option_tag_list()}
              | {error, Error},
      Error :: no_header
             | {invalid_option_tag_list, term()}.

-spec build(HeaderName :: binary(), option_tag_list()) -> ersip_hdr:header().
build(HdrName, {option_tag_list, _} = OptionTagList) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([assemble(OptionTagList)], Hdr).

-spec assemble(option_tag_list()) ->  iolist().
assemble({option_tag_list, _} = OptionTagList) ->
    ersip_iolist:join(<<", ">>,
                      [ersip_option_tag:to_binary(OptionTag)
                       || OptionTag <- to_list(OptionTagList)
                      ]).

'''