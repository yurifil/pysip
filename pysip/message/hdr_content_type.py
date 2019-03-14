from pysip import PySIPException
from pysip.message.hdr import Header, BaseSipHeader
from pysip.message.parser_aux import parse_token, parse_slash, parse_params


class MIMEType(object):
    MIME = 'mime'

    def __init__(self, type, subtype):
        self.type = type
        self.subtype = subtype

    def __eq__(self, other):
        if isinstance(other, MIMEType):
            return self.type == other.type and self.subtype == other.subtype


class ContentTypeHeaderError(PySIPException):
    pass


class ContentTypeHeader(BaseSipHeader):
    def __init__(self, header=None):
        self.mime_type = None
        self.params = None
        if header is not None:
            self.mime_type, self.params = ContentTypeHeader.parse_content_type(header)

    def __eq__(self, other):
        if isinstance(other, ContentTypeHeader):
            return self.mime_type == other.mime_type and self.params == other.params
        return NotImplemented

    @staticmethod
    def parse(header):
        return ContentTypeHeader(header)

    @staticmethod
    def parse_content_type(header):
        if isinstance(header, str):
            return ContentTypeHeader.parse_content_type_string(header)
        elif isinstance(header, Header):
            return ContentTypeHeader.parse_header_object(header)
        else:
            raise ContentTypeHeaderError(f'Cannot parse header {header}: type of object is {type(header)}')

    @staticmethod
    def parse_content_type_string(header):
        try:
            content_type, rest = parse_token(header)
            slash, rest = parse_slash(rest)
            subtype, rest = parse_token(rest)
        except Exception as e:
            raise ContentTypeHeaderError(f'Cannot parse "{header}": invalid header ({e}).')
        if rest and rest.startswith(';'):
            params = parse_params(rest[1:], ';')
        else:
            if rest:
                raise ContentTypeHeaderError(f'Cannot parse "{header}": invalid parameters.')
            params = None
        return MIMEType(content_type, subtype), params

    @staticmethod
    def parse_header_object(header):
        raw_values = header.values.copy()
        if not raw_values:
            raise ContentTypeHeaderError(f'Cannot parse header "{header}": no content type.')
        else:
            return ContentTypeHeader.parse_content_type_string(''.join(header.values))

    def assemble(self):
        ret_val = f'{self.mime_type.type}/{self.mime_type.subtype}'
        if self.params is not None:
            for name, value in self.params:
                ret_val = f'{ret_val};{name}={value}'
        return ret_val

    def build(self, header_name='Content-Type'):
        hdr = Header(header_name)
        hdr.add_value(self.assemble())
        return hdr
