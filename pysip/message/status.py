from pysip import FINAL, PROVISIONAL
from pysip import PySIPException, HeaderError


REASON_DICT = {100: 'Trying',
               200: 'OK',
               404: 'Not Found',
               405: 'Method Not Allowed',
               408: 'Request Timeout',
               416: 'Unsupported URI Scheme',
               420: 'Bad Extension',
               423: 'Interval Too Brief',
               483: 'Too many hops'}

UNKNOWN_STATUS = 'Unknown Status'


def response_type(status_code):
    if status_code in range(200, 700):
        return FINAL
    elif status_code in range(100, 200):
        return PROVISIONAL
    else:
        raise PySIPException(f'Status code {status_code} is out of range 100..699')


def reason_phrase(code):
    return REASON_DICT.get(code, UNKNOWN_STATUS)


