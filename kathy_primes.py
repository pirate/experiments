def get_primes(max_val):
    numbers = range(max_val + 1)
    primes = [1]
    for i in numbers[2:]:
        prime = True
        for j in primes[1:]:
            if not i % j:
                prime = False
                break
        if prime:
            primes.append(i)
            yield i

def test_primes():
    assert list(get_primes(3)) == [1, 2, 3]
