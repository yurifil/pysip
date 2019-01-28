from pysip.message.hparams import HParams, HParamsError, HParamNotFound
from pysip.message.qvalue import QValue
import pytest


def test_parse_raw_single_param():
    hparams = HParams('a=b')
    assert hparams.find_raw(param='A') == 'b'


def test_parse_raw_two_params():
    hparams = HParams('a=b ; c=d')
    assert hparams.find_raw(param='c') == 'd'


def test_parse_raw_no_value_param():
    hparams = HParams('a=b ; c')
    assert hparams.find_raw('c') is None


@pytest.mark.parametrize('value', ['@=@', '@', 'a=b;@', '@;a=b'])
def test_parse_raw_error(value):
    with pytest.raises(HParamsError):
        HParams(value)


def test_find_raw():
    hparams = HParams('a=b')
    assert hparams.find_raw('A') == 'b'
    assert isinstance(hparams.find_raw('ab'), HParamNotFound)


def test_find_raw_set():
    hparams = HParams()
    hparams.set('expires', 3, 'Expires', '3')
    hparams.set('q', QValue('0.1'), 'q', '0.1')
    hparams.set_raw('MyCustomParam', None)
    assert hparams.find_raw('expires') == '3'
    assert isinstance(hparams.find_raw('q1'), HParamNotFound)
    assert hparams.find_raw('q') == '0.1'
    assert hparams.find_raw('mycustoMParam') is None


def test_set_raw():
    hparams = HParams()
    hparams.set_raw('a', 'b')
    hparams.set_raw('a', 'c')
    assert hparams.assemble() == 'a=c'
    assert hparams.find_raw('A') == 'c'


@pytest.mark.parametrize('value', ['a=b', 'a', 'a;b;c', 'A=b;b;c', 'A=192.168.1.1;b;c'])
def test_assemble(value):
    hparams = HParams(value)
    assert hparams.assemble() == value


def test_set_find():
    hparams = HParams()
    hparams.set('expires', 3, 'Expires', '3')
    assert hparams.assemble() == 'Expires=3'
    hparams.set('q', QValue('0.1'), 'q', '0.1')
    assert hparams.assemble() == 'Expires=3;q=0.1'
    hparams.set_raw('MyCustomParam', None)
    assert hparams.assemble() == 'Expires=3;q=0.1;MyCustomParam'
    assert hparams.find('expires') == 3
    assert hparams.find('Expires') == 3
    assert hparams.find('q') == QValue('0.1')
    assert isinstance(hparams.find('q1'), HParamNotFound)
    assert hparams.find_raw('expires') == '3'
