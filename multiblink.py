# Nick Sweeting 2013

# arduino setup:

from time import sleep

def digitalWrite(pin, value):
	value=bool(value)
	pin=int(pin)
	print("pin[%s]: %s" % (pin,value*"ON"+(not value)*"OFF"))

def delay(time):
	sleep(int(time))

HIGH = True
LOW = False

#####################################

ledpin1 = 10
ledpin2 = 9

freq1 = 120.0
freq2 = 60.0

delay1 = (1.0/freq1)/2 		# period = 1/120Hz = 8.3ms (cut in half because the power has to cycle twice in that time)
delay2 = (1.0/freq2)/2

while True:														# void loop {

	if delay1 > delay2:											# if (delay1 > delay2) {
		delay(delay1)											# 	delay(delay1);
		digitalWrite(ledpin1, HIGH)								#	digitalWrite(ledpin1, HIGH);
		delay(delay2-delay1)									#	delay(delay2-delay1);
		digitalWrite(ledpin2, HIGH)								#	digitalWrite(ledpin2, HIGH);