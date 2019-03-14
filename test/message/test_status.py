from pysip.message.status import response_type, reason_phrase
from pysip.message.sip_message import bad_request_reason, SipHeaderError
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
        phrase = reason_phrase(code)
        assert text == phrase
    test_phrase(100, "Trying")
    test_phrase(404, "Not Found")
    test_phrase(408, "Request Timeout")
    test_phrase(199, "Unknown Status")


def test_bad_request_reason():
    def test_bad_request_reason(match_text, error):
        reason = bad_request_reason(error).lower()
        print(match_text.lower(), reason)
        assert re.search(match_text.lower(), reason)
    test_bad_request_reason("Bad Request", ImportError()),
    error = SipHeaderError()
    error.set_header('max-forwards')
    test_bad_request_reason("Max-Forwards", error)
