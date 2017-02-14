def traverse_nest(iterable: list):
    """return items from left to right from an arbitrarily deeply nested list
       [[1, 2], [2, [4, 2]], 3] -> [1, 2, 2, 4, 2, 3]
    """

    for item in iterable:
        if isinstance(item, (list, tuple)):  # could be replaced with duck-typing for generality
            yield from traverse_nest(item)
        else:
            yield item


assert list(traverse_nest([])) == []
assert list(traverse_nest([1, 2, 3])) == [1, 2, 3]
assert list(traverse_nest([[1, 2], [2, [4, 2]], 3])) == [1, 2, 2, 4, 2, 3]


def getNext(generator, default=None):
    return next(generator, default)

def hasNext(generator, item):
    getNext(generator) == item

test_generator = traverse_nest([[1], [2, [4, 2]], 3])
assert getNext(test_generator) == 1
assert getNext(test_generator) == 2
assert hasNext(test_generator, 4)
assert hasNext(test_generator, 2)
