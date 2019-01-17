from pysip import FINAL, PROVISIONAL
from pysip import PySIPException, HeaderError


REASON_DICT = {100: b'Trying',
               200: b'OK',
               404: b'Not Found',
               405: b'Method Not Allowed',
               408: b'Request Timeout',
               416: b'Unsupported URI Scheme',
               420: b'Bad Extension',
               423: b'Interval Too Brief',
               483: b'Too many hops'}

UNKNOWN_STATUS = b'Unknown Status'


def response_type(status_code):
    if status_code in range(200, 700):
        return FINAL
    elif status_code in range(100, 200):
        return PROVISIONAL
    else:
        raise PySIPException(f'Status code {status_code} is out of range 100..699')


def reason_phrase(code):
    return REASON_DICT.get(code, UNKNOWN_STATUS)


def bad_request_reason(error):
    if isinstance(error, HeaderError):
        return b'Invalid ' + error.header + b' number'
    else:
        return b'Bad request'

