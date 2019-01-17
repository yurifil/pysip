import array


SMALL_ALPHA_SIZE = ord('z') - ord('a') + 1
CAP_ALPHA_SIZE = ord('Z') - ord('A') + 1
ALPHA_SIZE = SMALL_ALPHA_SIZE + CAP_ALPHA_SIZE
DIGIT_SIZE = 9 - 0 + 1
OTHER_SIZE = 9
DIGIT_SHIFT = 9
SMALL_ALPHA_SHIFT = DIGIT_SHIFT + DIGIT_SIZE
CAP_ALPHA_SHIFT = SMALL_ALPHA_SHIFT + SMALL_ALPHA_SIZE


class CharTable(object):
    size = ALPHA_SIZE + DIGIT_SIZE + OTHER_SIZE
    char_map = {0: '-',
                1: '.',
                2: '!',
                3: '*',
                4: '_',
                5: '+',
                6: '`',
                7: "'",
                8: '~'}

    @classmethod
    def token_translate(cls, x):
        if x in cls.char_map:
            return cls.char_map.get(x)
        if DIGIT_SHIFT <= x < SMALL_ALPHA_SHIFT:
            return x - DIGIT_SHIFT + ord('0')
        if SMALL_ALPHA_SHIFT <= x < CAP_ALPHA_SHIFT:
            return x - SMALL_ALPHA_SHIFT + ord('a')
        if CAP_ALPHA_SHIFT <= x < CAP_ALPHA_SHIFT + CAP_ALPHA_SIZE:
            return x - CAP_ALPHA_SHIFT + ord('A')

    @classmethod
    def tfun(cls, x):
        cls.token_translate(x)


def token(binary):
    return encode(binary, CharTable)


def encode(binary, token_tab):
    return encode_impl(binary, token_tab, 0, [])


def encode_impl(binary, char_table, non_neg_integer, bytes_list):
    if binary == b'' and non_neg_integer == 0:
        return array.array('B', bytes_list)
    #if binary == b'' and non_neg_integer < char_table.size:
    #    return array.array('B', [char_table.tfun(non_neg_integer)] + bytes_list)
    #elif non_neg_integer >= char_table.size:
    #    pass
    #elif non_neg_integer <= char_table.size:
    #    pass

