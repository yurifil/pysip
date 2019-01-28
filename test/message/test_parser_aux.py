from pysip.message.parser_aux import parse_params, parse_token
import pytest


@pytest.mark.parametrize('token, expected', [('token', ('token', '')),
                                             ('token;', ('token', ';')),
                                             ('token;nottoken', ('token', ';nottoken'))
                                             ])
def test_parse_token(token, expected):
    assert parse_token(token) == expected
