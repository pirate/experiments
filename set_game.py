# Nick Sweeting 2016/02/28
# Set the game: https://en.wikipedia.org/wiki/Set_(game)

from itertools import combinations

TRAITS = {
    'color': {'red', 'green', 'purple'},
    'number': {1, 2, 3},
    'fill': {'solid', 'dashed', 'empty'},
    'shape': {'oval', 'diamond', 'squiggle'},
}

class Card(object):
    def __init__(self, number, color, fill, shape):
        assert number in TRAITS['number']
        assert color in TRAITS['color']
        assert fill in TRAITS['fill']
        assert shape in TRAITS['shape']
        self.number = number
        self.color = color
        self.fill = fill
        self.shape = shape

    def __str__(self):
        return self.__repr__()

    def __repr__(self):
        shape = {'oval': '0', 'diamond': 'D', 'squiggle': 'S'}[self.shape]
        return '%s (%s %s)' % (shape * self.number, self.fill, self.color)

    def __eq__(self, card):
        return self.number == card.number and \
               self.color == card.color and \
               self.fill == card.fill and \
               self.shape == card.shape


def all_same(trait_set):
    return len(trait_set) == 1

def all_different(trait_set):
    return len(trait_set) == 3

def is_set(cards):
    for trait in TRAITS.keys():
        trait_list = {getattr(card, trait) for card in cards}
        if not (all_same(trait_list) or all_different(trait_list)):
            return False
    return True


class Board(object):
    def __init__(self, cards=None):
        self.cards = cards

    def find_sets(self):
        for cards in combinations(self.cards, 3):
            if is_set(cards):
                yield cards

    def __str__(self):
        return str(list(self.cards))


def test_set():
    card1 = Card(3, 'red', 'dashed', 'squiggle')
    card2 = Card(3, 'red', 'empty', 'squiggle')
    card3 = Card(3, 'red', 'solid', 'squiggle')
    assert is_set((card1, card2, card3))
    card3.number = 2
    assert not is_set((card1, card2, card3))
    assert Card(3, 'red', 'solid', 'squiggle') == Card(3, 'red', 'solid', 'squiggle')
    return True

if __name__ == '__main__':
    test_set()

    # shortcuts for inputting boards quickly
    R, G, P = 'red', 'green', 'purple'
    D, S, E = 'dashed', 'solid', 'empty'
    SS, OO, DD = 'squiggle', 'oval', 'diamond'

    cards = (
        (3, R, D, SS), (2, G, S, OO), (1, R, S, OO), (2, R, S, OO),
        (1, P, D, DD), (2, P, D, SS), (1, G, S, DD), (2, R, E, SS),
        (2, R, D, SS), (1, G, S, SS), (2, R, E, OO), (2, G, E, SS),
    )
    board = Board([Card(*traits) for traits in cards])
    print board
    print set(board.find_sets())



# alternatively:

# from itertools import combinations

# lines = open("input.txt").read().splitlines()

# for cards in combinations(lines, 3):
#     if any(len(set(card[i] for card in cards)) == 2 for i in range(4)):
#         continue
#     for card in cards:
#         print(card, end=" ")
#     print()
