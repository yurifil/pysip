from collections import namedtuple
from pysip.message.method import invite, bye, ack, cancel

MethodSet = namedtuple('method_set', 'method_set')


def new(method_list):
    return MethodSet(set(method_list))


def has(method, method_set):
    return method in method_set


def to_list(method_set):
    return list(method_set.method_set)


def invite_set():
    return new([invite(), bye(), ack(), cancel()])

