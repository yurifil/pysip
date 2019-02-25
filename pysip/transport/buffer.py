from pysip import PySIPException
from pysip.transport import MoreDataRequired
from collections import deque


class TransportBufferException(PySIPException):
    pass


def new_dgram(dgram_str):
    state = State()
    state.add(dgram_str)
    state.eof = True
    return state


def new(options):
    return State(options)


class State(object):
    def __init__(self, options=None):
        if options is None:
            options = dict()
        self.options = options
        self.acc = ''
        self.acc_length = 0
        self.queue = deque()
        self.queue_length = 0
        self.eof = False
        self.pos = 0

    @property
    def length(self):
        return self.acc_length + self.queue_length

    def read_till_crlf(self):
        return self.read_till('\r\n', len(self.acc))

    def add(self, string):
        self.queue.append(string)
        self.queue_length += len(string)

    def read_till(self, pattern, position):
        if position < 0:
            return self.read_till(pattern, 0)
        start = self.acc.find(pattern, position)
        if start < 0:
            if len(self.queue) == 0:
                return MoreDataRequired(state=self)
            else:
                item = self.queue.popleft()
                self.queue_length = self.queue_length - len(item)
                pos = len(self.acc) - len(pattern) + 1
                self.acc += str(item)
                self.acc_length = len(self.acc)
                return self.read_till(pattern, pos)
        else:
            rest_pos = start + len(pattern)
            rest_len = len(self.acc) - rest_pos
            if rest_len != 0:
                self.queue.appendleft(self.acc[rest_pos:rest_pos+rest_len])
            q_len = self.queue_length + rest_len
            new_pos = self.pos + rest_pos
            found_str = self.acc[0:start]
            self.queue_length = q_len
            self.acc = ''
            self.acc_length = 0
            self.pos = new_pos
            return found_str

    def has_eof(self):
        return self.eof

    def read(self, length):
        return self.read_more_to_acc(length-self.acc_length)

    def read_more_to_acc(self, length):
        if length == 0:
            ret_val = self.acc
            self.acc = ''
            self.pos += self.acc_length
            self.acc_length = 0
            return ret_val
        try:
            item = str(self.queue.popleft())
        except IndexError:
            return MoreDataRequired(self)
        size = len(item)
        q_len = self.queue_length - size
        if size <= length:
            total_acc_length = self.acc_length + size
            self.acc += item
            self.acc_length = total_acc_length
            self.queue_length = q_len
            return self.read_more_to_acc(length - size)
        else:
            h_part = item[0:length]
            r_part = item[length:]
            self.queue.append(r_part)
            self.acc += h_part
            self.queue_length = q_len + len(r_part)
            self.acc_length += length
            return self.read_more_to_acc(0)




'''
-spec read_more_to_acc(Len :: non_neg_integer(), state()) -> Result when
      Result :: {ok, iolist(), state()}
              | {more_data, state()}.
read_more_to_acc(Len, #state{acc = <<>>} = State) ->
    read_more_to_acc(Len, State#state{acc = []});
read_more_to_acc(0, #state{acc = Acc, acclen = AccLen, pos = Pos} = State) ->
    State_ = State#state{acc = <<>>,
                         acclen = 0,
                         pos = Pos + AccLen
                        },
    {ok, lists:reverse(Acc), State_};
read_more_to_acc(Len, #state{acc = Acc, acclen = AccLen} = State) ->
    Queue = ?queue(State),
    QueueLen = ?queuelen(State),
    case queue:out(Queue) of
        {empty, Queue} ->
            {more_data, State};
        {{value, V}, Q} ->
            Size = byte_size(V),
            QLen = QueueLen - Size,
            case Size of
                Sz when Sz =< Len ->
                    TotalAcc = AccLen + Sz,
                    State1 = State#state{acc = [V | Acc],
                                         acclen = TotalAcc,
                                         queue = Q,
                                         queuelen = QLen
                                        },
                    read_more_to_acc(Len-Sz, State1);
                Sz when Sz > Len ->
                    HPart = binary:part(V, 0, Len),
                    RPart = binary:part(V, Len, Sz-Len),
                    Q1    = queue:in_r(RPart, Q),
                    Q1Len = QLen + byte_size(RPart),
                    State1 = State#state{acc = lists:reverse([HPart | Acc]),
                                         acclen = AccLen + Len,
                                         queue = Q1,
                                         queuelen = Q1Len
                                        },
                    read_more_to_acc(0, State1)
            end
    end.

%% @doc Read number of bytes from the buffer.
-spec read(Len :: pos_integer(), state()) -> Result when
      Result :: {ok, iolist(), state()}
              | {more_data, state()}.
read(Len, #state{acclen = AccLen} = State) when Len >= AccLen->
    read_more_to_acc(Len-AccLen, State).

-spec read_till(Pattern, Position, state()) -> Result when
      Pattern  :: binary(),
      Position :: integer(),
      Result :: {ok, binary(), state()}
              | {more_data, state()}.
read_till(Pattern, Pos, State) when Pos < 0 ->
    read_till(Pattern, 0, State);
read_till(Pattern, Pos, #state{acc = A} = State) ->
    Queue = ?queue(State),
    QueueLen = ?queuelen(State),
    case binary:match(A, Pattern, [{scope, {Pos, byte_size(A) - Pos}}]) of
        nomatch ->
            case queue:is_empty(Queue) of
                true ->
                    {more_data, State};
                false ->
                    {{value, Item}, Q} = queue:out(Queue),
                    QLen = QueueLen - byte_size(Item),
                    Acc_ = <<A/binary, Item/binary>>,
                    Pos_ = byte_size(A) - byte_size(Pattern) + 1,
                    State_ = State#state{queue    = Q,
                                         queuelen = QLen,
                                         acc      = Acc_,
                                         acclen   = byte_size(Acc_)
                                        },
                    read_till(Pattern, Pos_, State_)
            end;
        {Start, Len} ->
            RestPos = Start + Len,
            RestLen = byte_size(A) - RestPos,
            Q =
                case RestLen of
                    0 ->
                        Queue;
                    _ ->
                        queue:in_r(binary:part(A, RestPos, RestLen), Queue)
                end,
            QLen = QueueLen + RestLen,
            NewBufPos = State#state.pos + RestPos,
            State_ = State#state{queue    = Q,
                                 queuelen = QLen,
                                 acc      = <<>>,
                                 acclen   = 0,
                                 pos      = NewBufPos
                                },
            {ok, binary:part(A, 0, Start), State_}
    end.




%% @doc Reads buffer until CRLF is found.  CRLF is not included in
%% output and skipped on next read.
-spec read_till_crlf(state()) -> Result when
      Result :: {ok, binary(), state()}
              | {more_data, state()}.
read_till_crlf(#state{acc = A} = State) ->
    read_till(?crlf, byte_size(A)-1, State).

%% @doc Put raw binary to the buffer.
-spec add(binary(), state()) -> state().
add(Binary, #state{eof = false, queuelen = Len} = State) when is_binary(Binary) ->
    Q =    queue:in(Binary, ?queue(State)),
    QLen = Len + byte_size(Binary),
    State#state{queue    = Q,
                queuelen = QLen
               }.

new(Options) ->
    #state{options = Options}.

%% @doc New buffer with datagram.
-spec new_dgram(binary()) -> state().
new_dgram(Binary) ->
    S0 = new(#{}),
    S1 = add(Binary, S0),
    set_eof(S1).


%%%===================================================================
%%% Types
%%%===================================================================

-type options() :: map().
-record(state,{options  = #{}          :: options(),
               acc      = <<>>         :: binary() | iolist(),
               acclen   = 0            :: non_neg_integer(),
               queue    = queue:new()  :: queue:queue(binary()),
               queuelen = 0            :: non_neg_integer(),
               eof      = false        :: boolean(),
               %% Current position in the buffer (beginning of acc).
               pos      = 0            :: non_neg_integer()
              }).

-type state() :: #state{}.

-define(crlf, <<$\r,$\n>>).
-define(queue(State), State#state.queue).
-define(queuelen(State), State#state.queuelen).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc New buffer with specified options.
%% Note options are reserved in API for future use.
-spec new(options()) -> state().




%% @doc Add eof to the buffer
-spec set_eof(state()) -> state().
set_eof(State) ->
    State#state{eof=true}.

%% @doc Buffer has EOF
-spec has_eof(state()) -> boolean().
has_eof(#state{eof=EOF}) ->
    EOF.

%% @doc number of bytes accumulated inside the buffer.
-spec length(state()) -> non_neg_integer().
length(#state{acclen = AccLen, queuelen = QLen}) ->
    QLen + AccLen.

%% @doc Current position in the stream (number of stream bytes read
%% out from this buffer).
-spec stream_postion(state()) -> non_neg_integer().
stream_postion(#state{pos = Pos}) ->
    Pos.





%%%===================================================================
%%% internal implementation
%%%===================================================================

%% @doc Read from buffer till Patter. Position is starting search
%% position in accamulator. May be optimized in part of Acc
%% concatenation.
%%
%% @private


'''