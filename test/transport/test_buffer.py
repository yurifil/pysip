from pysip.transport.buffer import new, new_dgram
from pysip.transport import MoreDataRequired


def test_read_till_crlf():
    buf = new({})
    buf.add('a\r\nb')
    assert buf.read_till_crlf() == 'a'
    assert isinstance(buf.read_till_crlf(), MoreDataRequired)
    buf.add('\r')
    assert isinstance(buf.read_till_crlf(), MoreDataRequired)
    buf.add('\n')
    assert buf.read_till_crlf() == 'b'


def test_read_till_crlf_2():
    buf = new({})
    buf.add('aaa\r\nbbb\r\n')
    assert buf.read_till_crlf() == 'aaa'
    assert buf.read_till_crlf() == 'bbb'


def test_read_till_crlf_3():
    buf = new({})
    buf.add('a')
    assert isinstance(buf.read_till_crlf(), MoreDataRequired)
    buf.add('b')
    buf.add('\r\n')
    assert buf.read_till_crlf() == 'ab'


def test_read():
    buf = new({})
    buf.add('a')
    assert buf.read(1) == 'a'
    buf.add('abc')
    assert isinstance(buf.read(5), MoreDataRequired)
    buf.add('12')
    assert buf.read(5) == 'abc12'
    buf.add('123456789')
    assert buf.read(2) == '12'
    assert buf.read(3) == '345'
    assert buf.read(4) == '6789'


def test_stream_position():
    buf = new({})
    assert buf.pos == 0
    buf.add('ab\r')
    assert buf.pos == 0
    assert buf.read(1) == 'a'
    assert buf.pos == 1
    buf.add('\nef\r\n')
    assert buf.pos == 1
    assert buf.read_till_crlf() == 'b'
    assert buf.pos == 4
    assert buf.read_till_crlf() == 'ef'
    assert buf.pos == 8


def test_length():
    buf = new({})
    assert buf.length == 0
    buf.add('ab\r')
    assert buf.length == 3
    assert buf.read(1) == 'a'
    assert buf.length == 2
    buf.add('\nef\r\n')
    assert buf.length == 7
    assert buf.read_till_crlf() == 'b'
    assert buf.length == 4
    assert buf.read_till_crlf() == 'ef'
    assert buf.length == 0
