# -*- coding: utf-8 -*-
# Nick Sweeting ® 2013
# scans images to find which colors are in the images, returns percentages sorted in descending order of RGB (Hex) colors and their english equivalents

from PIL import Image

def colorscan(img):
    pix_ar = img.load() # load image into 2D array
    cols = img.size[0]  # number of columns
    rows = img.size[1]  # number of rows

    colorlist = []

    for row in range(rows): # our nested loop
        for col in range(cols):
            colorlist.append(str(pix_ar[col, row][0])+"-"+str(pix_ar[col, row][1])+"-"+str(pix_ar[col, row][2]))

    colorvalues = {}

    for color in colorlist:
        colorvalues[color] = 0

    for color in colorlist:
        colorvalues[color] += 1

    return colorvalues

if __name__ == '__main__':
    input_image = Image.open('image.png')

    result = colorscan(input_image)

    print result
