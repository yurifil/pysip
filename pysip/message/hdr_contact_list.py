from pysip import PySIPException
from pysip.message.hdr import Header, BaseSipHeader
from pysip.message.hdr_contact import ContactHeader


STAR = '*'


class ContactHeaderListError(PySIPException):
    pass


class ContactHeaderList(BaseSipHeader):
    def __init__(self, contact_list=None):
        self.contact_list = list()
        if contact_list is not None:
            self.contact_list = self.parse_contact_list(contact_list)

    def __repr__(self):
        return self.assemble()

    @property
    def values(self):
        return self.contact_list

    @staticmethod
    def parse(contact_list):
        return ContactHeaderList(contact_list)

    def assemble(self):
        return self.build('Contact').serialize_to_string()

    def build(self, header_name):
        hdr = Header(header_name)
        if self.is_star():
            hdr.add_value(STAR)
        else:
            for contact in self.contact_list:
                hdr.add_value(contact.assemble())
        return hdr

    def is_star(self):
        return self.contact_list == STAR

    @staticmethod
    def add_to_contact_list(contact_list_string, accumulator):
        if contact_list_string.startswith(STAR):
            raise ContactHeaderListError(f'Cannot parse contact list {contact_list_string}: multiple contacts and '
                                         f'star are invalid')
        try:
            contact = ContactHeader(contact_list_string)
        except Exception as e:
            raise ContactHeaderListError(f'Cannot parse contact list {contact_list_string}: {e}')
        accumulator.append(contact)
        if not contact.rest.lstrip():
            return accumulator
        if contact.rest.startswith(','):
            ContactHeaderList.add_to_contact_list(contact.rest[1:], accumulator)

    @staticmethod
    def parse_contact_list(contact_list):
        ret_val = list()
        if isinstance(contact_list, Header):
            if contact_list.values[0] == STAR:
                return STAR
            for value in contact_list.values:
                if value == STAR:
                    raise ContactHeaderListError(f'Cannot parse contact list {contact_list}: multiple contacts and '
                                                 f'star are invalid')
                ContactHeaderList.add_to_contact_list(value, ret_val)
        else:
            raise ContactHeaderListError(f'Cannot parse contact list {contact_list}: should be of type Header')
        return ret_val


'''
parse(Header) ->
    MaybeRevContactList =
        lists:foldl(fun(IOContact, Acc) ->
                            add_to_maybe_contact_list(iolist_to_binary(IOContact), Acc)
                    end,
                    {ok, []},
                    ersip_hdr:raw_values(Header)),
    case MaybeRevContactList of
        {ok, star} ->
            {ok, star};
        {ok, RevContactList} ->
            {ok, lists:reverse(RevContactList)};
        Error ->
            Error
    end.

-spec add_to_maybe_contact_list(binary(), maybe_rev_contact_list()) -> maybe_rev_contact_list().
add_to_maybe_contact_list(_, {error, _} = Error) ->
    Error;
add_to_maybe_contact_list(<<"*">>, {ok, []}) ->
    {ok, star};
add_to_maybe_contact_list(_, {ok, star}) ->
    {error, {invalid_contact, <<"multiple contacts and star are invalid">>}};
add_to_maybe_contact_list(<<>>, {ok, _} = Result) ->
    Result;
add_to_maybe_contact_list(Bin, {ok, ContactList}) when is_list(ContactList) ->
    case ersip_hdr_contact:parse_hdr(Bin) of
        {ok, Contact, Rest0} ->
            case ersip_bin:trim_head_lws(Rest0) of
                <<>> ->
                    {ok, [Contact | ContactList]};
                <<",", Rest1/binary>> ->
                    Rest2 = ersip_bin:trim_head_lws(Rest1),
                    add_to_maybe_contact_list(Rest2, {ok, [Contact | ContactList]})
            end;
        {error, _} = Error ->
            io:format("add_to_maybe_contact_list: ~p", [Bin]),
            Error
    end.

-spec build(HeaderName :: binary(), contact_list()) -> ersip_hdr:header().
build(HdrName, star) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(<<"*">>, Hdr);
build(HdrName, ContactList) when is_list(ContactList) ->
    Hdr = ersip_hdr:new(HdrName),
    lists:foldl(
      fun(Contact, HdrAcc) ->
              ersip_hdr:add_value(ersip_hdr_contact:assemble(Contact), HdrAcc)
      end,
      Hdr,
      ContactList).

-module(ersip_hdr_contact_list).

-export([make/1,
         make_star/0,
         build/2,
         parse/1
        ]).

-export_type([contact_list/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type star_contact() :: star.
-type contact_list() :: star_contact()
                      | [ersip_hdr_contact:contact()].

-type parse_result() :: {ok, contact_list()}
                      | {error, term()}.

-type maybe_rev_contact_list() :: {ok, contact_list()}
                                | {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec make(iolist() | binary()) -> contact_list().
make(Binary) ->
    H0 = ersip_hdr:new(<<"Contact">>),
    H1 = ersip_hdr:add_value(Binary, H0),
    case parse(H1) of
        {ok, ContactList} ->
            ContactList;
        {error, Reason} ->
            error(Reason)
    end.

-spec make_star() -> star_contact().
make_star() ->
    star.




%% Contact        =  ("Contact" / "m" ) HCOLON
%%                   ( STAR / (contact-param *(COMMA contact-param)))
-spec parse(ersip_hdr:header()) -> parse_result().

%%%===================================================================
%%% Internal implementation
%%%===================================================================


'''