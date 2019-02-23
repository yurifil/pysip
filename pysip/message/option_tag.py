from pysip import PySIPException
from pysip.message.parser_aux import check_token


class OptionTagError(PySIPException):
    pass


class OptionTag(object):
    def __init__(self, tag=None):
        self.tag = tag
        if tag is not None:
            self.tag = self.parse(tag)

    @staticmethod
    def parse(tag):
        if check_token(tag):
            return tag
        raise OptionTagError(f'Cannot parse tag {tag}: not a token.')
