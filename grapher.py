# -*- coding: utf-8 -*-

import sys

title = "The Graph"
points = ((15,3),(24,4),(1, 6),(28,2))

def graph(points, title="Graph", symbol="+"):
	xmax = 0
	ymax = 0
	for point in points:
		if point[0] > xmax:
			xmax = point[0]
		if point[1] > ymax:
			ymax = point[1]

	graph = []
	row = []

	for x in range(0,xmax):
		row.append("   ")
	for y in range(0,ymax):
		graph.append(row[:]) # shorthand to spawn a new list to prevent changes to *row from changing all the rows (immutable/vs mutable vars)
	for point in points:
		graph[point[1]-1][point[0]-1] = " %s " % symbol

	sys.stdout.write(" "*((len(graph[0])*3/2)-len(title)/2)+title+"\n") # math for centering title
	sys.stdout.write("   y\n")

	for idx, row in enumerate(reversed(graph)):
		sys.stdout.write("%s |" % str(len(graph)-idx).ljust(2))
		for point in row:
			sys.stdout.write(point)
		sys.stdout.write("\n")
	sys.stdout.write("0  +"+xmax*"–––"+"x\n")
	sys.stdout.write("  0 ")
	for idx in range(1, len(graph[0])+1):
		sys.stdout.write(" "+str(idx).ljust(2))

if __name__ == "__main__":
	graph(points, title, "penis")