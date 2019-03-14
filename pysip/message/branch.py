from secrets import token_bytes
from pysip.message.pysip_id import token
from pysip.binary import to_lower
import re

rfc3261_branch_rx = re.compile('z9hG4bK.*')
rfc3261_branch_key_rx = re.compile('z9hg4bk.*')


class BranchError(ValueError):
    pass


class BranchKeyError(ValueError):
    pass


class Branch(object):
    def __init__(self, string):
        if not isinstance(string, str):
            raise BranchError
        self.binary = string

    def __eq__(self, other):
        if isinstance(other, (Branch, BranchKey)):
            return BranchKey(self) == BranchKey(other)
        return NotImplemented

    def assemble(self):
        return self.binary

    def make_key(self):
        return BranchKey(self)

    def is_rfc3261(self):
        return is_rfc3261(self)


class BranchKey(object):
    ENCODING = 'utf-8'

    def __init__(self, branch):
        if not isinstance(branch, (Branch, BranchKey)):
            raise BranchKeyError
        self.binary = branch.binary.lower()

    def __eq__(self, other):
        if isinstance(other, BranchKey):
            return self.binary == other.binary
        return NotImplemented


class BranchBytes(object):
    def __init__(self, binary):
        if not isinstance(binary, bytes):
            raise BranchError
        self.binary = binary

    def __eq__(self, other):
        if isinstance(other, (Branch, BranchKey)):
            return BranchKey(self) == BranchKey(other)
        return NotImplemented


class BranchKeyBytes(object):
    ENCODING = 'utf-8'

    def __init__(self, branch):
        if not isinstance(branch, (Branch, BranchKey)):
            raise BranchKeyError
        self.binary = to_lower(branch.binary, encoding=self.ENCODING)

    def __eq__(self, other):
        if isinstance(other, BranchKey):
            return self.binary == other.binary
        return NotImplemented


def make_rfc3261(binary):
    if is_rfc3261(Branch(binary)):
        return Branch(binary)
    else:
        return Branch('z9hG4bK' + binary)


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
        return BranchKey(Branch)
    elif isinstance(branch, BranchKey):
        return branch


def make_random(num_bytes):
    make_rfc3261(token(token_bytes(num_bytes)))
