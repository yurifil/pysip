from pysip.message.hnames import ALL_KNOWN_HEADERS


class HeaderRequired:
    ALL = 'all'
    OPTIONAL = 'optional'
    WITH_BODY = 'with_body'
    REQUESTS = 'requests'


def all_known_headers():
    return ALL_KNOWN_HEADERS


def parse_header(header, msg):
    pass


def copy_header(header, src_msg, dst_msg):
    pass


def copy_headers(header_list, src_msg, dst_msg):
    pass


def set_header(header, value, msg):
    pass


def set_raw_header(header, msg):
    pass


def remove_header(header, msg):
    pass


def parse_header_by_descr(descr, header):
    pass


def get_header(header, descr, msg):
    pass


def is_required(msg, required):
    pass


def copy_raw_header(header, src_msg, dst_msg):
    pass


def header_descr(header):
    pass
