from collections import namedtuple
from pysip.message.method import invite, bye, ack, cancel


class MethodSet(object):
    def __init__(self, method_list):
        self.method_set = set(method_list)

    def has(self, method):
        return method in self.method_set

    def to_list(self):
        return list(self.method_set)

    @staticmethod
    def invite_set():
        return MethodSet([invite(), bye(), ack(), cancel()])
