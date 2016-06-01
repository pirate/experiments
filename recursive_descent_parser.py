import re

# mapping of terminal token type to regex of characters that match that type
TOKEN_TYPES = {
    name: re.compile(pattern) for name, pattern in
    {
        'INT': r'^\d+$',
        'OPEN': r'^\($',
        'CLOSE': r'^\)$',
        'PLUS': r'^\+$',
        'TIMES': r'^\*$',
    }.items()
}

# grammar that describes the accepted sequences
# current token : [possible next token types]
GRAMMAR = {
    'START': {'INT', 'OPEN'},
    'INT': {'INT', 'CLOSE', 'PLUS', 'TIMES', 'END'},
    'OPEN': {'INT'},
    'CLOSE': {'PLUS', 'TIMES', 'END'},
    'PLUS': {'INT', 'OPEN'},
    'TIMES': {'INT', 'OPEN'},
    'END': {},
}


def is_token(token_type: str, token: str, token_types=TOKEN_TYPES) -> bool:
    """check that a string token matches a given token type regex"""

    assert token_type in token_types, 'Unrecognized token type %s' % token_type
    return bool(token_types[token_type].match(token))

def test_is_token():
    assert is_token('INT', '5'), "Token type INT didn't match 5"
    assert not is_token('INT', '+'), "Token type INT matched match +"
    assert is_token('OPEN', '('), "Token type OPEN didn't match ("
    assert not is_token('OPEN', ')'), "Token type OPEN matched )"
    assert is_token('PLUS', '+'), "Token type PLUS didn't match +"
    assert not is_token('PLUS', '*'), "Token type PLUS matched *"
    assert is_token('TIMES', '*'), "Token type TIMES didn't match *"
    assert not is_token('TIMES', '+'), "Token type TIMES matched +"


class InputConsumer(object):
    """A stepper to store current position in a string buffer to aid parsing"""

    def __init__(self, input_stream: str='', start_pos=0) -> None:
        self.input_stream = input_stream
        self.current_idx = start_pos

    def at(self, idx: int=None):
        idx = idx if idx is not None else self.current_idx
        if idx > len(self.input_stream) - 1:
            return ''
        return self.input_stream[idx]

    def step(self) -> None:
        self.current_idx += 1

    def back(self) -> None:
        self.current_idx -= 1

    def __str__(self):
        before = self.input_stream[:self.current_idx]
        curr = self.input_stream[self.current_idx]
        after = self.input_stream[self.current_idx + 1:]
        return '{0}[{1}]{2}'.format(before, curr, after)

    def __repr__(self):
        return str(self)


class LexerNode(object):
    """A linked list of lexed nodes storing node type, token str, next, and prev"""

    def __init__(self, token: str, token_type: str=None,
                 prev_node: LexerNode=None, next_node: LexerNode=None) -> None:
        self.token = token
        self.token_type = token_type
        self.prev_node = prev_node
        self.next_node = next_node


    def __str__(self) -> str:
        return '<{0}{1}> {2}'.format(
            self.token_type,
            ': {0}'.format(self.token) if self.token else '',
            self.next_node or '',
        )

    def __repr__(self) -> str:
        return str(self)

    def __eq__(self: LexerNode, other: object) -> bool:
        if not isinstance(other, LexerNode):
            return NotImplemented
        return (
            self.token == other.token and
            self.token_type == other.token_type and
            self.next_node == other.next_node
        )


def lex(input_stream: str) -> LexerNode:
    """Lex an input string into a LexerNode chain of tokens"""

    stream = InputConsumer(input_stream)
    lexed_chain = LexerNode('', 'START')
    current_node = lexed_chain

    while stream.at():
        current_token = stream.at()
        token_type = None

        for name in TOKEN_TYPES.keys():
            if is_token(name, current_token):
                token_type = name
                break

        stream.step()
        new_node = LexerNode(current_token, token_type, current_node)
        current_node.next_node = new_node
        current_node = new_node

    end_node = LexerNode('', 'END', current_node)
    current_node.next_node = end_node
    current_node = end_node

    return lexed_chain


def validate_lex(lexed_chain: LexerNode, grammar=GRAMMAR) -> bool:
    """Confirm that a lexed chain follows a given grammar"""

    current_node = lexed_chain
    while current_node.next_node:
        token_type = current_node.token_type
        next_token_type = current_node.next_node.token_type

        if next_token_type not in grammar[token_type]:
            return False

        current_node = current_node.next_node

    return True

def test_lexing():
    true_cases = ['5', '55', '(5)',
                  '(5+5)', '5+5', '(5)+5', '5+(5)',
                  '(5)+(5)', '(5)+5+(5)',
                  '5)+5+(5']  # this case is an invalid parse, but a valid lex

    for case in true_cases:
        chain = lex(case)
        assert validate_lex(chain), 'Got invalid lex for {0} => {1}'.format(case, chain)

    false_cases = ['(', ')', '()', ')(', '((', '))',
                   '+', '5+', '+5',
                   '(+)', '(+', '+)', ')+', '+(', ')+(',
                   '5+5+', '+5+5+', '+5+5']

    for case in false_cases:
        chain = lex(case)
        assert not validate_lex(chain), 'Got valid lex for {0} => {1}'.format(case, chain)


if __name__ == '__main__':
    test_is_token()
    test_lexing()
