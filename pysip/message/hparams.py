from pysip import PySIPException
from pysip.message.parser_aux import parse_params, ParserAUXError


class HParamsError(PySIPException):
    pass


class HParamNotFound(object):
    pass


class HParams(object):
    PARSED_NAMES_KEY = 'parsed_names'
    LOWER_KEYS_KEY = 'keys'

    def __init__(self, params=None):
        self.order = list()
        self.orig = dict()
        self.parsed = {HParams.PARSED_NAMES_KEY: {}, HParams.LOWER_KEYS_KEY: {}}
        if isinstance(params, str):
            self.parse_raw(param_string=params)

    def set_parsed(self, lower_key, parsed_name, parsed_value):
        self.parsed[HParams.PARSED_NAMES_KEY][parsed_name] = (lower_key, parsed_value)
        self.parsed[HParams.LOWER_KEYS_KEY][lower_key] = parsed_name

    def parse_known(self, known_function):
        for lower_key in self.order:
            raw_key, raw_value = self.orig.get(lower_key)
            parsed_key, parsed_value = known_function(lower_key, raw_value)
            if parsed_key is not None:
                self.set_parsed(lower_key, parsed_key, parsed_value)

    def parse_raw(self, param_string):
        try:
            for param_key, param_value in parse_params(param_string, ';'):
                self.set_raw(param_key, param_value)
        except ParserAUXError as e:
            raise HParamsError(f'Cannot parse raw param string {param_string}: {e}')

    def set_raw(self, key, value):
        if key.lower() not in self.orig:
            self.order.append(key.lower())
            self.orig[key.lower()] = (key, value)
        else:
            self.orig[key.lower()] = (key, value)

    def find_raw(self, param):
        if isinstance(param, str):
            key_value_tuple = self.orig.get(param.lower())
            if key_value_tuple is not None:
                return key_value_tuple[1]
            else:
                return HParamNotFound()
        return NotImplemented

    def set(self, parsed_name, parsed_value, orig_key, orig_value):
        if parsed_name not in self.parsed[HParams.PARSED_NAMES_KEY]:
            self.order.append(orig_key.lower())
            self.orig[orig_key.lower()] = (orig_key, orig_value)
            self.parsed[HParams.PARSED_NAMES_KEY][parsed_name] = (orig_key.lower(), parsed_value)
            self.parsed[HParams.LOWER_KEYS_KEY][orig_key.lower()] = parsed_name
        else:
            lower_key, old_parsed_value = self.parsed[HParams.PARSED_NAMES_KEY][parsed_name]
            self.orig[lower_key] = (orig_key, orig_value)
            self.parsed[HParams.PARSED_NAMES_KEY][parsed_name] = (lower_key, parsed_value)

    def __repr__(self):
        return self.assemble()

    def assemble(self):
        params = list()
        for lower_key in self.order:
            key, value = self.orig.get(lower_key)
            if value is None:
                params.append(key)
            else:
                params.append(f'{key}={value}')
        return ';'.join(params)

    def find(self, param_name):
        if param_name.lower() in self.parsed[HParams.PARSED_NAMES_KEY]:
            return self.parsed[HParams.PARSED_NAMES_KEY][param_name.lower()][1]
        else:
            return HParamNotFound()

    def to_list(self):
        params_list = []
        for param_name in self.order:
            value = self.find(param_name)
            if isinstance(value, HParamNotFound):
                value = self.orig.get(param_name)[1]
            params_list.append((param_name, value))
        return params_list

    def __eq__(self, other):
        if isinstance(other, HParams):
            return self.orig == other.orig and self.order == other.order



'''
-spec find(parsed_name(), hparams()) -> {ok, parsed_value()} | not_found.
find(ParsedName, #hparams{parsed = P}) when is_atom(ParsedName) ->
    case maps:find(ParsedName, P) of
        error        -> not_found;
        {ok, {_, V}} -> {ok, V}
    end;
find(Name, #hparams{parsed = P} = HP) when is_binary(Name) ->
    LowerKey = ersip_bin:to_lower(Name),
    case maps:find(LowerKey, P) of
        error -> not_found;
        {ok, ParsedName} when is_atom(ParsedName) ->
            find(ParsedName, HP)
    end.


-spec to_list(hparams()) -> Result when
      Result :: [Item],
      Item   :: {parsed_name(), parsed_value()}
              | {lower_key(),   orig_value()}.
to_list(#hparams{} = HParams) ->
    [begin
         case maps:find(LowerKey, HParams#hparams.parsed) of
             {ok, ParsedName} ->
                 {LowerKey, ParsedValue} = maps:get(ParsedName, HParams#hparams.parsed),
                 {ParsedName, ParsedValue};
             error ->
                 {_, Value} = maps:get(LowerKey, HParams#hparams.orig),
                 {LowerKey, Value}
         end
     end || LowerKey <- HParams#hparams.order].


-spec set(parsed_name(), parsed_value(), orig_key(), orig_value(), hparams()) -> hparams().
set(ParsedName, ParsedValue, Key, Value, #hparams{parsed = PMap} = HParams) ->
    case maps:find(ParsedName, PMap) of
        error ->
            LowerKey = ersip_bin:to_lower(Key),
            Order  = HParams#hparams.order ++ [LowerKey],
            Orig   = (HParams#hparams.orig)#{LowerKey => {Key, Value}},
            Parsed = PMap#{ParsedName => {LowerKey, ParsedValue},
                           LowerKey => ParsedName},
            HParams#hparams{order = Order,
                            orig  = Orig,
                            parsed = Parsed};
        {ok, {LowerKey, _}} ->
            Orig   = (HParams#hparams.orig)#{LowerKey => {Key, Value}},
            Parsed = PMap#{ParsedName => {LowerKey, ParsedValue}},
            HParams#hparams{orig = Orig, parsed = Parsed}
    end.

-spec find_raw(orig_key(), hparams()) -> {ok, orig_value()} | not_found.
find_raw(BinName, #hparams{orig = Orig}) when is_binary(BinName) ->
    case maps:find(ersip_bin:to_lower(BinName), Orig) of
        error -> not_found;
        {ok, {_, V}} -> {ok, V}
    end;
find_raw(ParsedName, #hparams{parsed = P, orig = Orig}) when is_atom(ParsedName) ->
    case maps:find(ParsedName, P) of
        error -> not_found;
        {ok, {LowerKey, _}} ->
            {ok, {_, V}} = maps:find(LowerKey, Orig),
            {ok, V}
    end.


-spec parse_known(parse_known_fun(), hparams()) -> {ok, hparams()} | {error, term()}.
parse_known(ParseKnownFun, #hparams{order = Order, orig = Orig} = HParams0) ->
    lists:foldl(fun(_LowerKey, {error, _} = Error) ->
                        Error;
                   (LowerKey, {ok, HParams}) ->
                        {_, OrigValue} = maps:get(LowerKey, Orig),
                        case ParseKnownFun(LowerKey, OrigValue) of
                            {ok, unknown} -> {ok, HParams};
                            {ok, {ParsedName, ParsedValue}} ->
                                {ok, set_parsed(LowerKey, ParsedName, ParsedValue, HParams)};
                            {error, _} = Error ->
                                Error
                        end
                end,
                {ok, HParams0},
                Order).

-module(ersip_hparams).

-export([new/0,
         parse_raw/1,
         parse_known/2,
         assemble/1,
         assemble_bin/1,
         to_list/1,
         find/2,
         find_raw/2,
         set_raw/3,
         set/5
        ]).
-export_type([hparams/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(hparams, {order  = []  :: [lower_key()],
                  orig   = #{} :: #{lower_key()   => {orig_key(),  orig_value()}},
                  parsed = #{} :: #{parsed_name() => {lower_key(), parsed_value()},
                                    lower_key()   => parsed_name()
                                   }
                 }).
-type hparams()      :: #hparams{}.
-type lower_key()    :: binary().
-type orig_key()     :: binary().
-type parsed_name()  :: atom().
-type parsed_value() :: term().
-type orig_value()   :: binary() | novalue.

-type parse_known_fun() :: fun((lower_key(), orig_value()) -> parse_known_fun_result()).
-type parse_known_fun_result() :: {ok, {parsed_name(), parsed_value()}}
                                | {ok, unknown}
                                | {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> hparams().
new() ->
    #hparams{}.

-spec parse_raw(binary()) -> ersip_parser_aux:parse_result(hparams()).


%% @doc Enrich parameters with parsed values.  ParseKnownFun should
%% return result of the parameter parsing (see type definition).





%%%===================================================================
%%% Internal Implementation
%%%===================================================================






'''
