from pysip.message.method import Method, MethodError
import pytest


def test_parse():
    m1 = Method('AAAA')
    m2 = Method('BBBB')
    assert m1 == Method('AAAA')
    assert m1 != m2
    with pytest.raises(MethodError):
        Method('  ')

