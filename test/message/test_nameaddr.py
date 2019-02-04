from pysip.message.nameaddr import NameAddress, NameAddressError
from pysip.uri.uri_parser import SIPUriParserUnicode
from pysip.uri.uri import Uri
import pytest

PARSER = SIPUriParserUnicode()


@pytest.mark.parametrize('na_str, host, user, display_name, rest',
                         [('"A. G. Bell" <sip:agb@bell-telephone.com> ;tag=a48s',
                           'bell-telephone.com', 'agb', '"A. G. Bell"', ' ;tag=a48s'),
                          ('Bob <sip:bob@biloxi.com>;tag=a6c85cf', 'biloxi.com', 'bob', 'Bob', ';tag=a6c85cf'),
                          ('Bob Smith <sip:bob-smith@biloxi.com>;tag=a6c85cf', 'biloxi.com', 'bob-smith',
                           ['Bob', 'Smith'], ';tag=a6c85cf'),
                          ('<sip:bob-smith@biloxi.com>;tag=a6c85cf', 'biloxi.com', 'bob-smith', '', ';tag=a6c85cf'),
                          ('sip:bob-smith@biloxi.com;tag=a6c85cf', 'biloxi.com', 'bob-smith', '', ';tag=a6c85cf'),
                          ('sip:bob-smith@biloxi.com', 'biloxi.com', 'bob-smith', '', '')
                          ])
def test_nameaddr_parse(na_str, host, user, display_name, rest):
    parsed_na = NameAddress(na_str)
    host = PARSER.validate_host(host)
    uri = Uri()
    uri.parser_impl = PARSER
    uri.user = user
    uri.host = host
    assert parsed_na.display_name == display_name
    assert parsed_na.uri == uri
    assert parsed_na.rest == rest


# TODO: add ?assertMatch({error, _}, ersip_nameaddr:parse(<<"\"", 16#c2, "\" <sip:bob-smith@biloxi.com>;tag=a6c85cf">>)), test
@pytest.mark.parametrize('na_str', ['', 'sip:bob-smith@biloxi.com>', '<sip:bob-smith@biloxi.com',
                                    '<sip:bob-smith@biloxi.->', '1.2.3.4', '?:1.2.3.4', '1.2.3.4;tag=a6c85cf'])
def test_nameaddr_parse_error(na_str):
    with pytest.raises(NameAddressError):
        NameAddress(na_str)
