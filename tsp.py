# -*- coding: utf-8 -*-

# Traveling Salesman

# y
# |									X
# |			X
# |		X
# |							X
# |
# |
# |X
# |				X								
# |
# |
# |		X               X
# |
# |									X
#  ---------------------------------- x
#
#

import random
import threading
import time
import math

cities = [[0,6],[3,10],[3,3],[5,5],[9,3],[11,9],[13,2],[13,11]]

def distance(cordA, cordB):
    return math.sqrt((cordA[0] - cordB[0])**2 + (cordA[1] - cordB[1])**2)

last = cities[0]
distances = []

for city in cities:
	distances.append(distance(last,city))
	last = city

print cities
print distances