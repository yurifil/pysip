from pysip.message.status import response_type, reason_phrase, bad_request_reason
from pysip import FINAL, PROVISIONAL, PySIPException, HeaderError
import pytest
import re


def test_response_type():
    assert response_type(100) == PROVISIONAL
    assert response_type(199) == PROVISIONAL
    assert response_type(200) == FINAL
    assert response_type(299) == FINAL
    assert response_type(300) == FINAL
    assert response_type(399) == FINAL
    assert response_type(400) == FINAL
    assert response_type(499) == FINAL
    assert response_type(500) == FINAL
    assert response_type(599) == FINAL
    assert response_type(600) == FINAL
    assert response_type(699) == FINAL
    with pytest.raises(PySIPException):
        response_type(99)
    with pytest.raises(PySIPException):
        response_type(700)
    with pytest.raises(PySIPException):
        response_type('a')


def test_reason_phrase():
    def test_phrase(code, text):
        text_bin = text.encode('utf-8')
        phrase = reason_phrase(code)
        assert text_bin == phrase
    test_phrase(100, "Trying")
    test_phrase(404, "Not Found")
    test_phrase(408, "Request Timeout")
    test_phrase(199, "Unknown Status")


def test_bad_request_reason():
    def test_bad_request_reason(match_text, error):
        reason = bad_request_reason(error).lower()
        print(match_text.lower(), reason)
        assert re.search(match_text.lower(), reason)
    test_bad_request_reason(b"Bad Request", ImportError()),
    test_bad_request_reason(b"Max-Forwards", HeaderError(header=b'max-forwards'))
