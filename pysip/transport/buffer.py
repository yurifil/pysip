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
