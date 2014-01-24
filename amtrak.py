# -*- coding: utf-8 -*-
# Spec for proposed traveling salesman algorith

# Step 1:

# loop through every point and calculate the distance & heading to all the surrounding points
# use elimination tables to prevent duplicate searches

# Step 2:

# then, start with a chosen starting point, and average the headings of the remaining points, then travel to the point with the least difference in heading, repeat for each point until they've all been visited
# if one merely wishes to traverse all the points and return to the starting point, simply set the destination point B as the farthest point from the starting one


# efficiency of this algorithm: O(nPn)


# INPUT:

points_input = [[3,10],[8,1],[3,5],[1,4],[8,10],[4,5]]

### Begin code

import math
import sys

import grapher

points_input = sorted(points_input, key=lambda x: x[1])                                     # sort points by y coordinates from least to greatest
dimensions = (sorted(points_input, key=lambda x: x[0])[-1][0] , points_input[-1][1])        # (max_x, max_y) = (sort by x then grab last (largest) x value, take y-sorted list and grab last (largest) value)
point_vectors = []

precision = 2

grapher.graph(points_input)
print("")


def dist(point1, point2):
    # euclidean distance formula d = √((ay-by)^2+(ax-bx)^2)
    return round(math.sqrt((point1[0]-point2[0])**2+(point1[1]-point2[1])**2), precision)

def heading(point1, point2, dist):
    if not dist: return 0
    elif point2[0] == point1[0]: return int(not (point2[1] > point1[1]))*180
    elif point2[1] == point1[1]: return int(not (point2[0] > point1[0]))*180+90
    else:
        return round(math.degrees(math.cos((point2[0]-point1[0])/dist)))


for c_idx, current_point in enumerate(points_input):
    point_vectors.append([])
    for o_idx, other_point in enumerate(points_input):
        if current_point != other_point:
            point_vectors[c_idx].append([0, dist(current_point, other_point)])
            point_vectors[c_idx][o_idx][0] = heading(current_point, other_point, point_vectors[c_idx][o_idx][1])
        else:
            point_vectors[c_idx].append([0, 0])


for idx, distances in enumerate(point_vectors):
    sys.stdout.write("Point: [%s,%s  direction,distance:  " % (points_input[idx][0], (str(points_input[idx][1])+"]").ljust(3)))
    for point in distances:
        sys.stdout.write(str(point).ljust(16))
    sys.stdout.write("\n")



# ######
#                B
#               /|
#              / |
#             /  |
#            /   |
#           /    |
#      5   /     | 4
#         /      |
#        /       |
#       /        |
#      /         |
#     /          |
#   A ------------
#           3
#
#  cos = adjacent/hypotenuse
#  angle(A) = degreees(cos(3/5))
