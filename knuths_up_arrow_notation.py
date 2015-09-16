def up(a, m, n):
    if n == 1:
        return a
    else:
        return a ** up(a, m, n-1)

fi

print up(4, 1, 3) == 64,        up(4, 1, 3)
print up(2, 3, 3) == 65536,     up(2, 3, 3)
print up(2, 2, 4) == 65536,     up(2, 2, 4)
print up(2, 2, 2) == 4,         up(2, 2, 2)
print up(2, 3, 2) == 4,         up(2, 3, 2)
print up(2, 2, 4) == 4,         up(2, 2, 4)
print up(2, 1, 1) == 2,         up(2, 1, 1)
print up(2, 2, 3) == 16,        up(2, 2, 3)

raise SystemExit(0)


# a ^(m) n = ?
# a, m, n = 2, 2, 4
# 2 ^^ 4 == 65536



# funcs = [
#     lambda a, m, n: reduce(lambda x,y:x*y, ((a ** m) for _ in xrange(n+1))),
#     lambda a, m, n: reduce(lambda x,y:x*y, ((a ** n) for _ in xrange(m+1))),
#     lambda a, m, n: reduce(lambda x,y:x*y, ((n ** a) for _ in xrange(m+1))),
#     lambda a, m, n: reduce(lambda x,y:x*y, ((m ** a) for _ in xrange(n+1))),
#     lambda a, m, n: reduce(lambda x,y:x*y, ((n ** m) for _ in xrange(a+1))),
#     lambda a, m, n: reduce(lambda x,y:x*y, ((m ** n) for _ in xrange(a+1))),
# ]

# # assert up(4, 0, 3) == 12



# print [a(4, 1, 3) == 64 for a in funcs]
# print [a(2, 3, 3) == 65536 for a in funcs]
# print [a(2, 2, 4) == 65536 for a in funcs]
# print [a(2, 2, 2) == 4 for a in funcs]
# print [a(2, 3, 2) == 4 for a in funcs]
# print [a(2, 2, 4) == 4 for a in funcs]
# print [a(2, 1, 1) == 2 for a in funcs]
# print [a(2, 2, 3) == 16 for a in funcs]

# assert up(4, 1, 3) == 64
# assert up(2, 3, 3) == 65536
# assert up(2, 2, 4) == 65536
# assert up(2, 2, 2) == 4
# assert up(2, 3, 2) == 4
# assert up(2, 2, 4) == 4
# assert up(2, 1, 1) == 2
# assert up(2, 2, 3) == 16
