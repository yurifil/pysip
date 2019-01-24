from pysip import PySIPException


def to_lower(binary, encoding='utf-8'):
    if isinstance(binary, bytes):
        return binary.decode(encoding).lower().encode(encoding)
    elif isinstance(binary, str):
        return binary.lower().encode(encoding)
    else:
        raise PySIPException(f'Cannot switch {binary} to lowercase: bytes and str types are supported but binary '
                             f'param is of type {type(binary)}')


def trim(binary, encoding='utf-8'):
    if isinstance(binary, bytes):
        return binary.decode(encoding).strip().encode(encoding)
    elif isinstance(binary, str):
        return binary.strip().encode(encoding)
    else:
        raise PySIPException(f'Cannot trim {binary}: bytes and str types are supported but binary param is '
                             f'of type {type(binary)}')


def to_integer(binary):
    if isinstance(binary, bytes):
        return int(binary.decode())
    elif isinstance(binary, str):
        return int(binary)
    elif isinstance(binary, int):
        return binary


def to_string(binary, encoding='utf-8'):
    if isinstance(binary, bytes):
        return binary.decode(encoding)
    elif isinstance(binary, str):
        return binary
    else:
        raise PySIPException(f'Cannot decode {binary} to string: bytes and str types are supported but binary param is '
                             f'of type {type(binary)}')


def to_bytes(string, encoding='utf-8'):
    if isinstance(string, str):
        return string.encode(encoding)
    elif isinstance(string, bytes):
        return string
    else:
        raise PySIPException(f'Cannot encode {string} to bytes: bytes and str types are supported but string param is '
                             f'of type {type(string)}')
