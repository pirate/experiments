# Nick Sweeting 2013
# flash two hypothetical LEDs at different speeds, while reading two seperate input dials in one thread

from time import sleep

# initial frequencies
freq1 = 120.0
freq2 = 60.0

delay1 = (1.0/freq1)/2      # period = 1/120Hz = 8.3ms (cut in half because the power has to cycle twice in that time)
delay2 = (1.0/freq2)/2

import sys



while True:             
    if sys.stdin.read() == "w":
        freq1 += 10
    elif sys.stdin.read() == "s":
        freq1 -= 10
    elif sys.stdin.read() == "e":
        freq2 += 10
    elif sys.stdin.read() == "d":
        freq2 -= 10

    delay1 = (1.0/freq1)/2
    delay2 = (1.0/freq2)/2

    if delay1 > delay2:
        sleep(delay2*.001)
        sys.stdout.write("O")
        sleep(delay1-delay2)
        sys.stdout.write("X")
    else:
        sleep(delay1*.001)
        sys.stdout.write("O")
        sleep(delay2-delay1)
        sys.stdout.write("X")
