from collections import namedtuple
from pysip.message.method import INVITE, BYE, ACK, CANCEL


class MethodSet(object):
    def __init__(self, method_list):
        self.method_dict = dict()
        for m in method_list:
            self.method_dict[m.method] = m

    def has(self, method):
        return method.method in self.method_dict

    def to_list(self):
        return self.method_dict.values()

    @staticmethod
    def invite_set():
        return MethodSet([INVITE, BYE, ACK, CANCEL])
