import random
# texas hold-em poker in python

suits = ('d','h','c','s')
ranks = (1,2,3,4,5,6,7,8,9,'J','Q','K','A')

deck = ["%s%s" % (r, s) for s in suits for r in ranks]

class Deck:
    idx = 1
    cards = deck[:]

    def shuffle(self):
        random.shuffle(self.cards)

round1 = Deck()

round1.shuffle()

print deck
# > ['1d', '2d', '3d', '4d', '5d', '6d', '7d', '8d', '9d', 'Jd', 'Qd', 'Kd', 'Ad', '1h', '2h', '3h', '4h', '5h', '6h', '7h', '8h', '9h', 'Jh', 'Qh', 'Kh', 'Ah', '1c', '2c', '3c', '4c', '5c', '6c', '7c', '8c', '9c', 'Jc', 'Qc', 'Kc', 'Ac', '1s', '2s', '3s', '4s', '5s', '6s', '7s', '8s', '9s', 'Js', 'Qs', 'Ks', 'As']
