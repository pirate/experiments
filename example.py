import string

def cleanup(phrase):                                        #defines function cleanup
    phrase = phrase.replace(' ','')                         #removes spaces
    phrase = phrase.lower();                                #change to lowercase
    for letter in phrase:                                   #remove punctuation
        if letter in string.punctuation:                    #remove punctuation
            phrase = phrase.replace(letter, '')             #remove punctuation
        return phrase                                       #return phrase

def encrypt(phrase, en_key):
    output = ""                                             #set output as nothing
    key_spot = 0                                            #set key_spot as nothing

    for position, c in enumerate(phrase):                   #enumerate: current position in character
        shift_amount = ord(en_key[key_spot])-96             #shift is key_spot - 96. So [0-255] scale --> [1-26]
        en_char = ord(phrase[position])-96+shift_amount     #en_char = plaintext_character + shift_amount
        if en_char > 26:                                    #if encrypted_character past 26, (z)
            en_char = en_char - 26                          #set it back to -26

        output += str(chr(en_char+96))                      #+96 to set numerical value back to computer-readable

        key_spot += 1                                       # move one letter down the key, or wrap to beginning of key if end is reached
        if key_spot >= len(en_key):
             key_spot = 0

    return output

choice = raw_input("e or d (e/d): ")
if choice == 'e':
    phrase = str(raw_input("Input a phrase to encrypt: "))  #input phrase
    phrase = cleanup(phrase)                                #calls on function cheanup for phrase
    en_key = str(raw_input("Input a key: "))                #assigns key to en_key
    en_key = cleanup(phrase)                                #cleans up en_key

    print encrypt(phrase, en_key)                           #calls on function encrypt for phrase, en_key
else:
    quit()
