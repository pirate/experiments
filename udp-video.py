'''two way UDP stream with interrupts every x packets for disconnections/sync issues'''


#Headers are data for keeping audio and video in sync

Stream 1: Headers
Stream 2: Video
Stream 3: Audio
Stream 4: Other (Data/Subtitles/ID3/Artwork)


'''The four are independent until they are compressed into a single packet stream and sent over the internet.'''


		1: ----------------#\
		2: -----------------#\------------\
server 	3: ------------------@---------------------[compression]---------UDP------>: Internet :<----------------> Client
		3: -----------------#/------------/
		5: ----------------#/


'''I'd imagine you could also do it by keeping the streams seperate but compressed, but it would lead to sync issues quickly if any packets are lost (due to using UDP).'''


'''implement in python'''

# Code beings here