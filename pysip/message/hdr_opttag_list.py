from pysip import PySIPException
from pysip.message.hdr import Header, BaseSipHeader
from pysip.message.parser_aux import check_token
from pysip.message.option_tag import OptionTag, OptionTagError


class OptTagListHeaderError(PySIPException):
    pass


class OptTagListHeader(BaseSipHeader):
    def __init__(self, opttaglist=None):
        self.opt_tag_set = set()
        if opttaglist is not None:
            self.opt_tag_set = self.parse_set(opttaglist)

    def __eq__(self, other):
        if isinstance(other, OptTagListHeader):
            return self.opt_tag_set == other.opt_tag_set
        return NotImplemented

    def __repr__(self):
        return f'{self.__class__.__name__}(opt_tag_set={self.opt_tag_set})'

    @staticmethod
    def parse_set(opttaglist):
        if isinstance(opttaglist, Header):
            if len(opttaglist.values) == 0:
                raise OptTagListHeaderError(f'Cannot parse opptaglist {opttaglist}: no values')
            else:
                ret_opttaglist = set()
                for val in opttaglist.values:
                    try:
                        opt_tag = OptionTag(val)
                    except OptionTagError:
                        raise OptTagListHeaderError(f'Cannot parse opptaglist {opttaglist}: value {val} is not token')
                    ret_opttaglist.add(opt_tag)
                return ret_opttaglist
        else:
            raise OptTagListHeaderError(f'Cannot parse opptaglist {opttaglist}: should be type Header not '
                                        f'{type(opttaglist)}')

    @staticmethod
    def parse(opttaglist):
        return OptTagListHeader(opttaglist)

    def intersect(self, other):
        if isinstance(other, OptTagListHeader):
            ret_val = OptTagListHeader()
            ret_val.opt_tag_set = self.opt_tag_set.intersection(other.opt_tag_set)
            return ret_val
        return NotImplemented

    def substract(self, other):
        if isinstance(other, OptTagListHeader):
            ret_val = OptTagListHeader()
            ret_val.opt_tag_set = self.opt_tag_set.difference(other.opt_tag_set)
            return ret_val
        return NotImplemented

    def assemble(self):
        return ', '.join([o.tag for o in self.opt_tag_set])

    def build(self, header_name):
        hdr = Header(header_name)
        hdr.add_value(self.assemble())
        return hdr

    @staticmethod
    def from_list(opttag_list):
        ret_val = OptTagListHeader()
        ret_val.opt_tag_set = set(opttag_list)
        return ret_val
