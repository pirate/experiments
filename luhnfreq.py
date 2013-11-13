# -*- coding: utf-8 -*-
# frequency of luhn-checksum-passing numbers between 1 & infinity

from grapher import graph

def cardLuhnChecksumIsValid(card_number):
    """ checks to make sure that a number passes a luhn mod-10 credit card checksum """
    card_number = ''.join(card_number.split())  # remove all whitespace
    sum = 0
    num_digits = len(card_number)
    oddeven = num_digits & 1
    for count in range(0, num_digits):
        digit = int(card_number[count])
        if not (( count & 1 ) ^ oddeven ):
            digit = digit * 2
        if digit > 9:
            digit = digit - 9
        sum = sum + digit
    return ( (sum % 10) == 0 )

print "1/10 of (Positive Real Integers) satisfy the luhn checksum."

for number in range(1,10):
    print "%s-%s=%s" % (10**(number-1),10**number,(10**number)-(10**(number-1)))
    results = 0
    for number in range(10**(number-1),10**number):
        if cardLuhnChecksumIsValid(str(number)):
            results = results+1
    print ": %s" % results

print results
