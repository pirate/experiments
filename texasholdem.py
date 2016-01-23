import random
# texas hold-em poker in python

suits = ('d','h','c','s')
ranks = (1,2,3,4,5,6,7,8,9,'J','Q','K','A')

deck = ["%s%s" % (r, s) for s in suits for r in ranks]

class Deck:
    idx = 1
    cards = deck[:]

    def shuffle(self):
# >   File "<stdin>", line 1
# >     def shuffle(self):
# >     ^
# > IndentationError: unexpected indent
        random.shuffle(self.cards)

round1 = Deck()

round1.shuffle()

print deck
