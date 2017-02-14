def fizzbuzz(a):
    return (a, "Fizz", "Buzz", "FizzBuzz")[(0x1241843 >> ((a % 15) * 2)) & 3]

map(fizzbuzz,xrange(1,100))


# Credits to: James @hackerschool
