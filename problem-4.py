def form_largest(nums):
    nums = [str(n) for n in nums]
    nums.sort(lambda x, y: (-1, 1)[int(x+y) < int(y+x)])
    return int(''.join(nums))

assert form_largest([50, 2, 1, 9]) == 95021
assert form_largest([420, 42, 423]) == 42423420
