# -*- coding: utf-8 -*-
# Nick Sweeting 2013
# MIT Liscence
# Instruction code for Eric Chen
# Lab 4: The Vigenere Cipher

# ╭━━━┳╮ ╭┳━━╮╭━━━┳━━━━┳━━━┳━━━╮
# ╰╮╭╮┃┃ ┃┃╭╮┃┃╭━╮┃╭╮╭╮┃╭━━┫╭━╮┃­
#  ┃┃┃┃┃ ┃┃╰╯╰┫╰━━╋╯┃┃╰┫╰━━┫╰━╯┃
#  ┃┃┃┃┃ ┃┃╭━╮┣━━╮┃ ┃┃ ┃╭━━┫╭━━╯
# ╭╯╰╯┃╰━╯┃╰━╯┃╰━╯┃ ┃┃ ┃╰━━┫┃
# ╰━━━┻━━━┻━━━┻━━━╯ ╰╯ ╰━━━┻╯

# to understand how the math in this program works, you have to understand how characters are stored in a computer
# in a computer, all ascii characters are stored a integers [0-255], so 'a' corresponds to 97, 'b' = 98, 'c' = 99, etc. (note lowercase and uppercase have different values so be consistent)
# since we want to shift the letters by certain amounts, you can do math on the characters by converting them to their integer values like this:
# ord('a') returns 97
# ord('b') returns 98 etc.
#
# anD vice versa:
# chr(97) returns 'a'
# chr(98) returns 'b'
# so to shift 'a' up one letter in the alphabet, you simply do chr(ord('a')+1)

# i use -96  because ord('a')-96 = 1, and a is the first letter of the alphabet/ if a is the key, ill want to shift everything 1 letter up, if b then 2 letters up, etc.
# ord(current_letter_in_key)-96 gives you the amount you want to shift by (then you use -26 if you shift farther than 26 because you have to wrap around back to a)

import string

def cleanup(phrase):
    phrase = phrase.replace(' ','')                            # remove spaces
    phrase = phrase.lower();                                   # change to lowercase
    for letter in phrase:                                      # remove other puctuation
        if letter in string.punctuation:
            phrase = phrase.replace(letter, '')
    return phrase

def encrypt(phrase, en_key):
    output = ""
    key_spot = 0

    for position,c in enumerate(phrase):                       # cool python trick that allows you to get the current position (index) of the character you're on
        shift_amount = ord(en_key[key_spot])-96                # math to convert chr value [0-255] into [1-26] for shift amount
        en_char = ord(phrase[position])-96+shift_amount        # encrypted_character = plaintext_character + shift_amount
        if en_char > 26:                                       # if encrypted_character > 26 (past z), -26 so that you start over again at a
            en_char = en_char - 26

        output += str(chr(en_char+96))                         # +96 to change my [1-26] value back into a character value (the way the computer stores it) (a=97, b=98, etc.)

        key_spot += 1                                          # move one letter down the key, or wrap to beginning of key if end is reached
        if key_spot >= len(en_key):
            key_spot = 0

    return output                                              # return the encrypted phrase

def generate_decryption_key(en_key):                           # code provided by instructor
    dec_key = ""
    for c in en_key.upper():
        dec_key += chr(90 - (ord(c) - 64)%26)                  # this fancy math is the same as doing -96 like I do below
    return dec_key.lower()                                     # the rest of my program uses lowercase, so i convert the key back into lowercase for consistency

def decrypt(phrase, dec_key):
    return encrypt(phrase, generate_decryption_key(dec_key))                                             # return the encrypted phrase

def crack(phrase):
    """coming soon"""
    return 0

choice = str(raw_input("Do you wish to encrypt or decrypt(e/d): "))
if choice == 'e':
    phrase = str(raw_input("Input a phrase to encrypt: "))
    phrase = cleanup(phrase)
    en_key = str(raw_input("Input a key: "))
    en_key = cleanup(en_key)

    print encrypt(phrase, en_key)
    print "Encrypting again using %s to get original, or use decryption option and input %s" % (generate_decryption_key(en_key), en_key)
else:
    phrase = str(raw_input("Input a phrase to decrypt: "))
    phrase = cleanup(phrase)
    en_key = str(raw_input("Input a key: "))
    en_key = cleanup(en_key)

    print decrypt(phrase, en_key)
