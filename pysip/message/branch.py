from collections import namedtuple
from secrets import token_bytes
from pysip import PySIPException
from pysip.message.pysip_id import token
import re

Branch = namedtuple('branch', 'binary')
BranchKey = namedtuple('branch_key', 'binary')

rfc3261_branch_rx = re.compile(b'z9hG4bK.*')
rfc3261_branch_key_rx = re.compile(b'z9hg4bk.*')


def make(binary):
    if isinstance(binary, bytes):
        return Branch(binary)
    else:
        raise PySIPException


def make_rfc3261(binary):
    if is_rfc3261(Branch(binary)):
        return Branch(binary)
    else:
        return Branch(b'z9hG4bK' + binary)


def is_rfc3261(branch):
    if isinstance(branch, Branch) and rfc3261_branch_rx.match(branch.binary):
        return True
    elif isinstance(branch, BranchKey) and rfc3261_branch_key_rx.match(branch.binary):
        return True
    else:
        return False


def assemble(branch):
    return branch.binary


def make_key(branch):
    if isinstance(branch, Branch):
        return BranchKey(branch.binary)
    elif isinstance(branch, BranchKey):
        return branch.binary


def make_random(num_bytes):
    make_rfc3261(token(token_bytes(num_bytes)))
