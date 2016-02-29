# -*- coding: utf-8 -*-
# http://xkcd.com/85/
import math
import sys
import re
from time import sleep

width        = int(sys.argv[1]) if len(sys.argv) > 1 else 25     # meters
height       = int(sys.argv[2]) if len(sys.argv) > 2 else width  # meters
walkingspeed = int(sys.argv[3]) if len(sys.argv) > 3 else 8      # km/h

def drawsquare(width, height):
    '''Prints a square showing the height to width ratio of a given square'''
    # TOP
    sys.stdout.write("╭")                   # sys.stdout.write is the same as print() but it doesnt add the newline after
    for number in range(0, width):
        sys.stdout.write("━━")
    sys.stdout.write("╮\n")

    # MIDDLE
    for number in range(0, height):
        sys.stdout.write("┃")
        for number in range(0, width):
            sys.stdout.write("  ")
        sys.stdout.write("┃\n")

    # BOTTOM
    sys.stdout.write("╰")
    for number in range(0, width):
        sys.stdout.write("━━")
    sys.stdout.write("╯\n\n")

def lengthcalc(width, height, cutpoint):
    '''Calculates the length in meters of the path which cuts accross the grass'''
    # e.g. width=20m, height=50m, cutpoint=0m,  length=53.8m
    #      width=20m, height=50m, cutpoint=10m, length=54.7m
    length = cutpoint + math.sqrt((height-cutpoint)**2 + width**2)
    return length

def timecalc(length, walkingspeed):
    '''Calculates the time in seconds that it would take to walk a given distance'''
    # e.g. 2km/h = 2000m in 60min
    # 1m:0.03min
    # 1m:1.8sec
    secondspermeter = (60.0/(walkingspeed*1000.0))*60.0
    return length*secondspermeter #returns seconds it takes to walk

def efficiencycalc(routetime, cuttime):
    '''Calculates how much more efficient the shorter path is than the longer one'''
    # routetime=100sec, cuttime=80sec, efficiency=80%
    return (cuttime/routetime)*100.0

def roundandunit(number, unit='', precision=2):
    '''Rounds numbers to a given decimal place and applies a unit (also deals with changing 2030m to 2.03km and 72min to 1hr12min)'''
    def prettify(num, precs):
        'round and convert num to string, then remove trailing zeroes'
        return re.sub("\\.?0+$", "", str(round(num, precs)))

    if unit == 'm':
        if number/1000.0 > 1.0:
            return prettify(number/1000.0, precision + 3) + 'km'
        else:
            return prettify(number, precision) + 'm'
    elif unit == 'sec':
        seconds = number
        hours = int(seconds/60.0/60.0)
        minutes = int(seconds/60.0 - hours*60.0)
        sec = int(seconds - hours*60.0*60.0 - minutes*60.0)
        if hours > 1:
            return "%sh %sm %ss" % (hours, minutes, sec)
        elif minutes > 1:
            return "%sm %ss" % (minutes, sec)
        else:
            return prettify(number, precision)
    else:
        # e.g. '%'
        return prettify(number, precision) + unit

sleep(0.1)

if width <= 100 and height <= 100:              # only draw the illustration if it'll fit on the screen
    drawsquare(width, height)

print("  Using width:     %sm" % width)
print("  Using height:    %sm" % height)
print("  Using speed:     %skm/h\n" % walkingspeed)

baselength = lengthcalc(width, height, height)
print("  Length of path without cutting accross the grass:    %s" % roundandunit(baselength,'m', 2))                 # full distance (all the way around the grass)
basetime = timecalc(baselength, walkingspeed)

print("  Time it would take you to walk that path:            %s\n" % roundandunit(basetime,'sec', 2))               # time it takes to walk the full distance (around the grass)
baseefficiency = efficiencycalc(basetime, basetime)

sleep(0.1)

print("Distance before cutting across     |      Efficiency boost       |       Total walking time       |       Total walking distance       |          Time saved")

lastefficiencyboost = 0
# calculate every possible path, starting with the full distance
# decreasing distance before cutting accross the grass by 1m each time

for dist in range(height, -1, -1):
    length = lengthcalc(width, height, dist)
    time = timecalc(length, walkingspeed)
    efficiencyboost = 100 - efficiencycalc(basetime, time)

    # only print if shows a change of more than 5% in time
    if ((efficiencyboost - lastefficiencyboost) > 5) or (dist == 0) or (dist == height):
        lastefficiencyboost = efficiencyboost

        dist = roundandunit(dist, 'm', 0)
        efficiencyboost = roundandunit(efficiencyboost, '%', 1)
        length = roundandunit(length, 'm', 0)
        timesaved = roundandunit(basetime-time, 'sec', 0)
        time = roundandunit(time, 'sec', 0)

        print("            %s+%s%s%s %ss" % (
            dist.ljust(33),
            efficiencyboost.ljust(33),
            time.ljust(33),
            length.ljust(37),
            timesaved,
        ))

        sleep(0.1)
