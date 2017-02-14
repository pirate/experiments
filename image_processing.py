from PIL import Image


def get_area(image, bottom_right, top_left=(0, 0)):
    return [
        image.getpixel((x, y))
        for y in range(top_left[0], bottom_right[0])
            for x in range(top_left[1], bottom_right[1])
    ]

def get_grayscale(r, g, b):
    """grayscale projection of an rgb pixel"""
    return 0.2989 * r + 0.5870 * g + 0.1140 * b

def get_average(pixels):
    """intensity of pixel in percent"""
    avg = sum(pixels) / len(pixels)
    return avg / 255 * 100

def get_intensity(filename, pixel_area=None):
    # 1. Load Image
    image = Image.open(filename).convert('RGB')

    # 2. Get Area of image
    if not pixel_area:
        bottom_right = image.height, image.width
        top_left = (0, 0)
    else:
        bottom_right, top_left = pixel_area

    pixel_area = get_area(image, bottom_right, top_left)

    # 3. Convert to grayscale
    grayscale_pixels = [get_grayscale(*pixel) for pixel in pixel_area]

    # 4. Calculate average intensity
    return get_average(grayscale_pixels)


if __name__ == '__main__':
    filename = 'photo.png'
    area = ((100, 200), (20, 50))   # bottom xy, top xy

    print('{} {}px to {}px average intensity is: {}%'.format(
        filename,
        area[1],
        area[0],
        get_intensity(filename, area),
    ))
    # photo.png (20, 50)px to (100, 200)px average intensity is: 36.9520728758%

    # To do lots of images, use a loop
    # for image in image_names:
    #     print(get_intensity(image))
