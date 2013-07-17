#!/usr/bin/python
#https://en.wikipedia.org/wiki/Collatz_conjecture

# Pseudocode:

# function hailstone(n)
# 	while n > 1
# 		show n
# 		if n is odd then
# 			set n = 3n + 1
# 		else
# 			set n = n / 2
# 		endif
# 	endwhile
# 	show n

# a = how many times the 3n+1 rule is used
# b = how many times the n/2 rule is used

from time import sleep
import os


def hailstone(n):
	a = 0
	b = 0
	result = []
	while n > 1:
		result.append(n)
		if n % 2 == 1:
			n = 3*n+1
			a += 1
		else:
			n = n/2
			b += 1
	result.append(n)
	return [result, a, b]


def run(numbers):
	for number in numbers:
		highest = 0
		stopping_points = hailstone(number)
		a = stopping_points[1]
		b = stopping_points[2]
		stopping_points = stopping_points[0]
		for point in stopping_points:
			if point > highest:
				highest = point
		number_of_points = len(stopping_points)
		print "num: %s #ofpoints: %s highest: %s 3n+1: %s n/2: %s points: %s" % (str(number).ljust(8), str(number_of_points).ljust(10), str(highest).ljust(10), str(a).ljust(5), str(b).ljust(5), str(stopping_points))
		sleep(0.01)

def export(numbers):
	c = open("export.txt", "wb")
	for number in numbers:
		highest = 0
		stopping_points = hailstone(number)
		a = stopping_points[1]
		b = stopping_points[2]
		stopping_points = stopping_points[0]
		for point in stopping_points:
			if point > highest:
				highest = point
		number_of_points = len(stopping_points)
		c.write("%s\t%s\t%s\n" % (number, a, b))
		

print "\n----------------------------range(0,20)------------------------------"
run(range(0,20))
print "\n-----------------------[0,1,2,3,6,12,24,48,96]-----------------------"
run([0,1,2,3,6,12,24,48,96])
print "\n----------------------[0,1,2,5,10,20,40,80,160]----------------------"
run([0,1,2,5,10,20,40,80,160])
print "\n----------------------[0,1,2,7,14,28,56,112,224]---------------------"
run([0,1,2,7,14,28,56,112,224])

export(range(0,2000))

# collatz conjecture is intrisically linked to understanding how prime numbers work
# its observed that every number n has a "prime root", where len(hailstone(n))-1 == len(hailstone(n/2))

# for example, take 7:
# num:7        # of points:17         highest:52         points:[7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]

# notice the pattern when comparing it to 14 (its double)

# num:14       # of points:18         highest:52         points:[14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]

# what about 28 ?

# num:28       # of points:19         highest:52         points:[28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]

# the pattern continues with all the primes:

# num:[3]prime # of points:8          highest:16         points:[3, 10, 5, 16, 8, 4, 2, 1]
# num:6        # of points:9          highest:16         points:[6, 3, 10, 5, 16, 8, 4, 2, 1]
# num:12       # of points:10         highest:16         points:[12, 6, 3, 10, 5, 16, 8, 4, 2, 1]
# num:24       # of points:11         highest:24         points:[24, 12, 6, 3, 10, 5, 16, 8, 4, 2, 1]
# num:48       # of points:12         highest:48         points:[48, 24, 12, 6, 3, 10, 5, 16, 8, 4, 2, 1]
# num:96       # of points:13         highest:96         points:[96, 48, 24, 12, 6, 3, 10, 5, 16, 8, 4, 2, 1]

# num:[5]prime # of points:6          highest:16         points:[5, 16, 8, 4, 2, 1]
# num:10       # of points:7          highest:16         points:[10, 5, 16, 8, 4, 2, 1]
# num:20       # of points:8          highest:20         points:[20, 10, 5, 16, 8, 4, 2, 1]
# num:40       # of points:9          highest:40         points:[40, 20, 10, 5, 16, 8, 4, 2, 1]
# num:80       # of points:10         highest:80         points:[80, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num:160      # of points:11         highest:160        points:[160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# 
# num:[7]prime # of points:17         highest:52         points:[7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num:14       # of points:18         highest:52         points:[14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num:28       # of points:19         highest:52         points:[28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num:56       # of points:20         highest:56         points:[56, 28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num:112      # of points:21         highest:112        points:[112, 56, 28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]

# graphs here and here clearly illustrate this effect by showing how each series returns to a root which can no longer be divided in two before being tripled+1
# https://en.wikipedia.org/wiki/File:CollatzConjectureGraphMaxValues.jpg
# >>> https://upload.wikimedia.org/wikipedia/commons/a/ad/Collatz-graph-all-30-no27.svg

# these numbers are special cases: 1, 2, 3, 6, 7, 9, 18, 25, 27, 54, 73, 97
# they take an abnormally large number of steps to sequence out relative to their value, take 27 in particular:
# num:26       # of points:11         highest:40         points:[26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num:27       # of points:112        highest:9232       points:[27, 82, 41, 124, 62, 31, 94, 47, 142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182, 91, 274, 137, 412, 206, 103, 310, 155, 466, 233, 700, 350, 175, 526, 263, 790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502, 251, 754, 377, 1132, 566, 283, 850, 425, 1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429, 7288, 3644, 1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 2308, 1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122, 61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num:28       # of points:19         highest:52         points:[28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]


# to prove my previous statement, ive added two stats, they show how many times each rule (3n+1 or n/2) is used per cycle (pretty obvious stuff):
# ----------------------------range(0,20)------------------------
# num: 0        #ofpoints: 1          highest: 0          3n+1: 0     n/2: 0     points: [0]
# num: 1        #ofpoints: 1          highest: 1          3n+1: 0     n/2: 0     points: [1]
# num: 2        #ofpoints: 2          highest: 2          3n+1: 0     n/2: 1     points: [2, 1]
# num: 3        #ofpoints: 8          highest: 16         3n+1: 2     n/2: 5     points: [3, 10, 5, 16, 8, 4, 2, 1]
# num: 4        #ofpoints: 3          highest: 4          3n+1: 0     n/2: 2     points: [4, 2, 1]
# num: 5        #ofpoints: 6          highest: 16         3n+1: 1     n/2: 4     points: [5, 16, 8, 4, 2, 1]
# num: 6        #ofpoints: 9          highest: 16         3n+1: 2     n/2: 6     points: [6, 3, 10, 5, 16, 8, 4, 2, 1]
# num: 7        #ofpoints: 17         highest: 52         3n+1: 5     n/2: 11    points: [7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num: 8        #ofpoints: 4          highest: 8          3n+1: 0     n/2: 3     points: [8, 4, 2, 1]
# num: 9        #ofpoints: 20         highest: 52         3n+1: 6     n/2: 13    points: [9, 28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num: 10       #ofpoints: 7          highest: 16         3n+1: 1     n/2: 5     points: [10, 5, 16, 8, 4, 2, 1]
# num: 11       #ofpoints: 15         highest: 52         3n+1: 4     n/2: 10    points: [11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num: 12       #ofpoints: 10         highest: 16         3n+1: 2     n/2: 7     points: [12, 6, 3, 10, 5, 16, 8, 4, 2, 1]
# num: 13       #ofpoints: 10         highest: 40         3n+1: 2     n/2: 7     points: [13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num: 14       #ofpoints: 18         highest: 52         3n+1: 5     n/2: 12    points: [14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num: 15       #ofpoints: 18         highest: 160        3n+1: 5     n/2: 12    points: [15, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num: 16       #ofpoints: 5          highest: 16         3n+1: 0     n/2: 4     points: [16, 8, 4, 2, 1]
# num: 17       #ofpoints: 13         highest: 52         3n+1: 3     n/2: 9     points: [17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num: 18       #ofpoints: 21         highest: 52         3n+1: 6     n/2: 14    points: [18, 9, 28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num: 19       #ofpoints: 21         highest: 88         3n+1: 6     n/2: 14    points: [19, 58, 29, 88, 44, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]

# -----------------------[3,6,12,24,48,96]-----------------------
# num: 3        #ofpoints: 8          highest: 16         3n+1: 2     n/2: 5     points: [3, 10, 5, 16, 8, 4, 2, 1]
# num: 6        #ofpoints: 9          highest: 16         3n+1: 2     n/2: 6     points: [6, 3, 10, 5, 16, 8, 4, 2, 1]
# num: 12       #ofpoints: 10         highest: 16         3n+1: 2     n/2: 7     points: [12, 6, 3, 10, 5, 16, 8, 4, 2, 1]
# num: 24       #ofpoints: 11         highest: 24         3n+1: 2     n/2: 8     points: [24, 12, 6, 3, 10, 5, 16, 8, 4, 2, 1]
# num: 48       #ofpoints: 12         highest: 48         3n+1: 2     n/2: 9     points: [48, 24, 12, 6, 3, 10, 5, 16, 8, 4, 2, 1]
# num: 96       #ofpoints: 13         highest: 96         3n+1: 2     n/2: 10    points: [96, 48, 24, 12, 6, 3, 10, 5, 16, 8, 4, 2, 1]

# ----------------------[5,10,20,40,80,160]----------------------
# num: 5        #ofpoints: 6          highest: 16         3n+1: 1     n/2: 4     points: [5, 16, 8, 4, 2, 1]
# num: 10       #ofpoints: 7          highest: 16         3n+1: 1     n/2: 5     points: [10, 5, 16, 8, 4, 2, 1]
# num: 20       #ofpoints: 8          highest: 20         3n+1: 1     n/2: 6     points: [20, 10, 5, 16, 8, 4, 2, 1]
# num: 40       #ofpoints: 9          highest: 40         3n+1: 1     n/2: 7     points: [40, 20, 10, 5, 16, 8, 4, 2, 1]
# num: 80       #ofpoints: 10         highest: 80         3n+1: 1     n/2: 8     points: [80, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num: 160      #ofpoints: 11         highest: 160        3n+1: 1     n/2: 9     points: [160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1]

# ----------------------[7,14,28,56,112,224]---------------------
# num: 7        #ofpoints: 17         highest: 52         3n+1: 5     n/2: 11    points: [7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num: 14       #ofpoints: 18         highest: 52         3n+1: 5     n/2: 12    points: [14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num: 28       #ofpoints: 19         highest: 52         3n+1: 5     n/2: 13    points: [28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num: 56       #ofpoints: 20         highest: 56         3n+1: 5     n/2: 14    points: [56, 28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num: 112      #ofpoints: 21         highest: 112        3n+1: 5     n/2: 15    points: [112, 56, 28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
# num: 224      #ofpoints: 22         highest: 224        3n+1: 5     n/2: 16    points: [224, 112, 56, 28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]


# the function to find the original number from a and b is n=

