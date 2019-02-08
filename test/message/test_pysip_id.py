from pysip.message.pysip_id import token
from pysip.message.parser_aux import check_token
import pytest

@pytest.mark.skip
def test_token():
    print(token(b'12345'))
    assert False


'''
token_test() ->
    ?assert(ersip_parser_aux:check_token(ersip_id:token(<<1,2,3,4,5>>))),
    [?assert(ersip_parser_aux:check_token(ersip_id:token(crypto:strong_rand_bytes(7)))) || _ <- lists:seq(1, 100)],
    ok.
'''