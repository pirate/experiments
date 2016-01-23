def calculate_primes(max_num):
    """sieve of eratosthenes to calculate primes up to max_num"""
    non_primes = set()
    for current in xrange(1, max_num + 1):
        if current not in non_primes: yield current
        for multiplier in xrange(2, current + 1):
            not_prime = current * multiplier
            if not_prime > max_num: break
            non_primes.add(not_prime)

def test_primes():
    assert list(calculate_primes(0)) == []
    assert list(calculate_primes(1)) == [1]
    assert list(calculate_primes(10)) == [1, 2, 3, 5, 7]
    p101 = set(calculate_primes(101))
    assert len(p101) == 27 and 99 not in p101 and 101 in p101

if __name__ == '__main__':
    test_primes()
    # print list(calculate_primes(10))
    # > [1, 2, 3, 5, 7]
    # print list(calculate_primes(101))
    # > [1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101]

    while True:
        primes = list(calculate_primes(int(raw_input("Primes up to: "))))
        print 'Primes: ', primes
        print 'Number of Primes: ', len(primes)
