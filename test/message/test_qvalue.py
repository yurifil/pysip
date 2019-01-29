from pysip.message.qvalue import QValue, QValueError
import pytest


@pytest.mark.parametrize('value1, value2', [('1', '1.0'),
                                            ('1', '1.'),
                                            ('1', '1.00'),
                                            ('1', '1.000'),
                                            ('0', '0.'),
                                            ('0', '0.0'),
                                            ('0', '0.00'),
                                            ('0', '0.000'),
                                            ('0.5', '0.500'),
                                            ('0.501', '0.501'),
                                            ('0.510', '0.51')
                                            ])
def test_equal(value1, value2):
    assert QValue(value1) == QValue(value2)


@pytest.mark.parametrize('value1, value2', [('0', '1.'),
                                            ('0', '0.001'),
                                            ('0.500', '0.501'),
                                            ('0.5', '0.501'),
                                            ('0.501', '0.502')
                                            ])
def test_comparison_lt(value1, value2):
    assert QValue(value1) < QValue(value2)


@pytest.mark.parametrize('value1, value2', [('0', '1.'),
                                            ('0', '0.001'),
                                            ('0.500', '0.501'),
                                            ('0.5', '0.501'),
                                            ('0.501', '0.502')
                                            ])
def test_comparison_gt(value1, value2):
    assert QValue(value2) > QValue(value1)


@pytest.mark.parametrize('value1, value2', [('0', '1.'),
                                            ('0', '0.001'),
                                            ('0.500', '0.501'),
                                            ('0.5', '0.501'),
                                            ('0.501', '0.502')
                                            ])
def test_comparison_le(value1, value2):
    assert QValue(value1) <= QValue(value2)


@pytest.mark.parametrize('value1, value2', [('0', '1.'),
                                            ('0', '0.001'),
                                            ('0.500', '0.501'),
                                            ('0.5', '0.501'),
                                            ('0.501', '0.502')
                                            ])
def test_comparison_ge(value1, value2):
    assert QValue(value2) >= QValue(value1)


@pytest.mark.parametrize('value', ['1.1', '-0', '0.abc', '1.001', '0.500a'])
def test_error(value):
    with pytest.raises(QValueError):
        QValue(value)


@pytest.mark.parametrize('value', ['0', '0.01', '1', '0.001', '0.999', '0.5', '0.501', '0.51', '0.499'])
def test_rebuild(value):
    qval1 = QValue(value)
    qval2 = QValue(qval1.qvalue)
    assert qval1 == qval2


def test_set():
    qval = QValue('0')
    assert '0.000' == qval.qvalue
    qval.set('1')
    assert '1' == qval.qvalue

