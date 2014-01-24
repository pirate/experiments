from pygame import *
import random

ballpic = image.load('ball.png')
ballpic.set_colorkey((0,0,0))

numballs = 10
delay = 5

done = False

balls = []

for count in range(numballs):
    balls.append(dict)
    balls[count] = {'x': 0, 'y': 0, 'xmove': random.randint(1, 2), 'ymove': random.randint(1, 2)}

init()
screen = display.set_mode((640, 480))
display.set_caption('Ball game')
event.set_grab(1)

while done == False:
    screen.fill(0)

    for count in range(numballs):
        screen.blit(ballpic, (balls[count]['x'], balls[count]['y']))

    display.update()

    time.delay(delay)

    for count in range(numballs):
        balls[count]['x'] = balls[count]['x'] + balls[count]['xmove']
        balls[count]['y'] = balls[count]['y'] + balls[count]['ymove']

    for count in range(numballs):
        if balls[count]['x'] > 620:
            balls[count]['xmove'] = random.randint(-2, 0)
        if balls[count]['x'] < -10:
            balls[count]['xmove'] = random.randint(0, 2)
        if balls[count]['y'] > 470:
            balls[count]['ymove'] = random.randint(-2, 0)
        if balls[count]['y'] < -10:
            balls[count]['ymove'] = random.randint(0, 2)

    for e in event.get():
        if e.type == KEYUP:
            if e.key == K_ESCAPE:
                done = True

    if screen.get_at((mouse.get_pos())) == (255, 255, 255, 255):
        done = True

print "You lasted for", time.get_ticks()/1000, "seconds!"
