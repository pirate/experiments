#! /usr/bin/python3
# -*- coding: utf-8 -*-

import random
import itertools

SUITS = ('♠', '♣', '♦', '♥')
RANKS = ('A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K')

class Deck(object):
    def __init__(self):
        self.cards = [r + s for r, s in itertools.product(RANKS, SUITS)]

    def __eq__(self, other):
        return self.cards == other.cards

    def __str__(self):
        return '[' + ','.join(self.cards) + ']'

    def __repr__(self):
        return '[' + self.peek().rjust(3) + ']]]'

    def faro_shuffle(self):
        '''Shuffles the deck using a perfect faro shuffle.'''
        shuffled = []
        for a, b in zip(self.cards[0:26], self.cards[26:]):
            shuffled.append(a)
            shuffled.append(b)
        self.cards = shuffled

    def riffle_shuffle(self):
        '''Shuffles the deck using a perfect alternating riffle'''
        shuffled = []
        for a, b in zip(self.cards[1::2], self.cards[0::2]):
            shuffled.append(a)
            shuffled.append(b)
        self.cards = shuffled

    def shuffle(self):
        random.shuffle(self.cards)

    def peek(self):
        return self.cards[0]


original_deck = Deck()
shuffled_deck = Deck()

for i in range(1, 10):
    shuffled_deck.faro_shuffle()
    if shuffled_deck == original_deck:
        print("Deck is back in new-deck order after %s shuffles." % i)
        break

for i in range(1, 4):
    shuffled_deck.riffle_shuffle()
    if shuffled_deck == original_deck:
        print("Deck is back in new-deck order after %s shuffles." % i)
        break

shuffled_deck.shuffle()
print(shuffled_deck.__repr__())
