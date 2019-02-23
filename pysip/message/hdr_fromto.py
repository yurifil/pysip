from pysip import PySIPException
from pysip.message.nameaddr import NameAddress
from pysip.message.parser_aux import check_token
from pysip.message.hdr import Header, BaseSipHeader
from pysip.message.hparams import HParams, HParamNotFound


TAG = 'tag'


class FromToError(PySIPException):
    pass


class FromToHeader(BaseSipHeader):
    DEFAULT_HOST = '127.0.0.1'

    def __init__(self, fromto=None):
        self.display_name = None
        self.uri = None
        self.params = HParams()
        if fromto is not None:
            self.display_name, self.uri, self.params = self.parse_fromto(fromto)

    def __eq__(self, other):
        if isinstance(other, FromToHeader):
            return self.display_name == other.display_name and self.uri == other.uri and self.params == other.params
        return NotImplemented

    def build(self, header_name):
        hdr = Header(header_name)
        hdr.add_value(self.assemble())
        return hdr

    @staticmethod
    def parse(fromto):
        ft = FromToHeader()
        ft.display_name, ft.uri, ft.params = FromToHeader.parse_fromto(fromto)
        return ft

    @staticmethod
    def parse_fromto(fromto):
        if isinstance(fromto, str):
            return FromToHeader.parse_string(fromto)
        elif isinstance(fromto, Header):
            return FromToHeader.parse_header(fromto)
        else:
            raise FromToError(f'Cannot parse fromto {fromto}: should be str or Header not {type(fromto)}')

    @staticmethod
    def parse_header(fromto):
        if not fromto.values:
            raise FromToError(f'Cannot parse fromto {fromto}: no values')
        elif len(fromto.values) > 1:
            raise FromToError(f'Cannot parse fromto {fromto}: multiple values')
        else:
            return FromToHeader.parse_string(fromto.values[0])

    @staticmethod
    def parse_string(fromto):
        nameaddr = NameAddress(fromto)
        params = FromToHeader.parse_params(nameaddr.rest.strip())
        return nameaddr.display_name, nameaddr.uri, params

    @staticmethod
    def parse_params(params_str):
        params = HParams()
        if params_str.startswith(';'):
            params = HParams(params_str[1:])
            tag = params.find_raw(TAG)
            if not isinstance(tag, HParamNotFound):
                if not check_token(tag):
                    raise FromToError(f'Cannot parse params {params_str}: tag value should be token.')
        return params

    @property
    def tag(self):
        tag = self.params.find_raw(TAG)
        if isinstance(tag, HParamNotFound):
            tag = None
        return tag

    @tag.setter
    def tag(self, value):
        self.params.set_raw(TAG, value)

    @property
    def tag_key(self):
        tag = self.tag
        if tag is None:
            return tag
        return tag.lower()

    def assemble(self):
        nameaddr = NameAddress.assemble(self.display_name, self.uri)
        params = self.params.assemble()
        if params:
            params = f';{params}'
        return f'{nameaddr}{params}'

