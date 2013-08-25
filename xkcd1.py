# -*- coding: utf-8 -*-
# http://xkcd.com/85/
import math
import sys
from time import sleep

width  = 25           # meters
height = 32          # meters
walkingspeed = 8        # km/h

def drawsquare(width, height):
    # TOP
    sys.stdout.write("╭")
    for number in range(0, width):
        sys.stdout.write("━")
    sys.stdout.write("╮\n")

    # MIDDLE
    for number in range(0, height):
        sys.stdout.write("┃")
        for number in range(0, width):
            sys.stdout.write(" ")
        sys.stdout.write("┃\n")

    # BOTTOM
    sys.stdout.write("╰")
    for number in range(0, width):
        sys.stdout.write("━")
    sys.stdout.write("╯\n\n")

def lengthcalc(width, height, cutpoint):
    # e.g. width=20m, height=50m, cutpoint=0m,  length=53.8m
    #      width=20m, height=50m, cutpoint=10m, length=54.7m
    length = cutpoint + math.sqrt((height-cutpoint)**2 + width**2)
    return length

def timecalc(length, walkingspeed):
    # e.g. 2km/h = 2000m in 60min
    # 1m:0.03min
    # 1m:1.8sec
    secondspermeter = (60.0/(walkingspeed*1000.0))*60.0
    return length*secondspermeter #returns seconds it takes to walk

def efficiencycalc(routetime, cuttime):
    # routetime=100sec, cuttime=80sec, efficiency=80%
    return (cuttime/routetime)*100.0

def roundandunit(number, unit='', precision=2):
    if unit == 'm':
        if number/1000.0 > 1.0:
            return str(round(number/1000.0,precision+3))+'km'
        else:
            return str(round(number,precision))+'m'
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
            return "%ss" % str(round(number,precision)).replace(".0","")
    elif unit == '%':
        return "%s%s" % (str(round(number,precision)), unit)
    else:
        return "%s%s" % (str(round(number,precision)).replace(".0",""), unit)

sleep(0.1)

if width <= 100 and height <= 100:
    drawsquare(width, height)

print "  Using width:     %sm" % width
print "  Using height:    %sm" % height
print "  Using speed:     %skm/h\n" % walkingspeed

baselength = lengthcalc(width, height, height)
print "  Non-Cut length:  %sm" % baselength
basetime = timecalc(baselength, walkingspeed)

print "  Non-Cut time:    %ssec\n" % basetime
baseefficiency = efficiencycalc(basetime, basetime)

sleep(0.1)

print "Distance before cutting across     |      Efficiency boost       |       Total walking time       |       Total walking distance       |          Time saved"

lastefficiencyboost = 0
for number in range(height, -1, -1):
    length = lengthcalc(width, height, number)
    time = timecalc(length, walkingspeed)
    efficiencyboost = 100-efficiencycalc(basetime, time)

    if ((efficiencyboost - lastefficiencyboost) > 5) or (number == 0) or (number == height): 
        lastefficiencyboost = efficiencyboost

        efficiencyboost = roundandunit(efficiencyboost,'%',1)
        length = roundandunit(length,'m',0)
        timesaved = roundandunit(basetime-time,'sec',0)
        time = roundandunit(time,'sec',0)

        print("            "+roundandunit(number,'m',0).ljust(33)+"+"+efficiencyboost.ljust(33)+time.ljust(33)+length.ljust(37)+timesaved)
        sleep(0.1)