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
