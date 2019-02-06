from pysip import PySIPException
from pysip.message.hdr import Header


class MaxForwardsHeaderError(PySIPException):
    pass


class MaxForwardsHeader(object):
    def __init__(self, maxforwards=None):
        self.maxforwards = None
        if maxforwards is not None:
            self.maxforwards = self.parse(maxforwards)

    def __eq__(self, other):
        if isinstance(other, MaxForwardsHeader):
            return self.maxforwards == other.maxforwards
        return NotImplemented

    @staticmethod
    def parse(maxforwards):
        if isinstance(maxforwards, int):
            maxforwards_int = maxforwards
        elif isinstance(maxforwards, str):
            try:
                maxforwards_int = int(maxforwards)
            except Exception as e:
                raise MaxForwardsHeaderError(f'Cannot parse maxforwards {maxforwards}: {e}')
        elif isinstance(maxforwards, Header):
            if len(maxforwards.values) == 1:
                return MaxForwardsHeader.parse(maxforwards.values[0])
            raise MaxForwardsHeaderError(f'Cannot parse maxforwards {maxforwards}: multivalues')
        else:
            raise MaxForwardsHeaderError(f'Cannot parse maxforwards {maxforwards}: should be type int, str or Header, '
                                         f'not {type(maxforwards)}')
        if maxforwards_int >= 0:
            return maxforwards_int
        else:
            raise MaxForwardsHeaderError(f'Cannot parse maxforwards {maxforwards}: should be non-negative int')

    def decrement(self):
        if self.maxforwards <= 0:
            raise MaxForwardsHeaderError(f'Cannot decrement maxforwards {self.maxforwards}: should be > 0')
        self.maxforwards -= 1
