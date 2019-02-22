from pysip.message.pysip_id import generate_token
from pysip.message.parser_aux import check_token


def test_token():
    assert check_token(generate_token(8))
