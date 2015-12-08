# lexical scoping
test_global = 1
g = globals()

def wat(x, y, z):
    g1 = g
    l = locals()
    closure = lambda: (x, y, l, g, g1)
    return closure

funcs = {
    'mul': lambda x, y, z: lambda: x * y,
    'div': lambda x, y, z: lambda: x / y,
    'add': lambda x, y, z: lambda: x + y,
    'sub': lambda x, y, z: lambda: x - y,
    'wut': lambda x, y, z: lambda: (x, y, globals()),
    'wat': wat,
}

test_global = 2

def calculator(method, x, y, z=0):
    closure = funcs[method](x, y, z)  # returns a lambda that takes no args because x, y were already defined
    
    print 'Closure for %s(%s, %s): %s' % (method, x, y, closure)
    print 'Attached scope: ', closure.func_code.co_freevars
    print 'Stack Size: ', closure.func_code.co_stacksize
    
    print 'Context:'
    for var in closure.func_closure:
        print var, ': ', var.cell_contents
    # z is not in the context of the closure because it's not needed in the add lambda, it's smart enough to only carry around scope that's used
    return closure()

test_global = 3

calculator('add', 3, 5)
# > Closure for add(3, 5): <function <lambda> at 0x1012d9500>
# > Attached scope:  ('x', 'y')
# > Stack Size:  2
# > Context:
# > <cell at 0x1012d86e0: int object at 0x7fe4a0c0c0c8> :  3
# > <cell at 0x1012d86a8: int object at 0x7fe4a0c0c098> :  5
# > 8
test_global = 4
calculator('wut', 3, 5)
# > Closure for wut(3, 5): <function <lambda> at 0x1012d9500>
# > Attached scope:  ('x', 'y')
# > Stack Size:  3
# > Context:
# > <cell at 0x1012d86a8: int object at 0x7fe4a0c0c0c8> :  3
# > <cell at 0x1012d86e0: int object at 0x7fe4a0c0c098> :  5
# > (3, 5, {'funcs': {'sub': <function <lambda> at 0x1012d9398>, 'wut': <function <lambda> at 0x1012d9410>, 'add': <function <lambda> at 0x1012d9320>, 'mul': <function <lambda> at 0x1012d9230>, 'div': <function <lambda> at 0x1012d92a8>, 'wat': <function wat at 0x1012d91b8>}, 'test_global': 4, 'g': {...}, '__builtins__': <module '__builtin__' (built-in)>, 'calculator': <function calculator at 0x1012d9488>, '__package__': None, '__name__': '__main__', 'wat': <function wat at 0x1012d91b8>, '__doc__': None})
test_global = 5
calculator('wat', 3, 5)
# > Closure for wat(3, 5): <function <lambda> at 0x1012d9500>
# > Attached scope:  ('g1', 'l', 'x', 'y')
# > Stack Size:  5
# > Context:
# > <cell at 0x1012d86e0: dict object at 0x1011b9168> :  {'funcs': {'sub': <function <lambda> at 0x1012d9398>, 'wut': <function <lambda> at 0x1012d9410>, 'add': <function <lambda> at 0x1012d9320>, 'mul': <function <lambda> at 0x1012d9230>, 'div': <function <lambda> at 0x1012d92a8>, 'wat': <function wat at 0x1012d91b8>}, 'test_global': 5, 'g': {...}, '__builtins__': <module '__builtin__' (built-in)>, 'calculator': <function calculator at 0x1012d9488>, '__package__': None, '__name__': '__main__', 'wat': <function wat at 0x1012d91b8>, '__doc__': None}
# > <cell at 0x1012d86a8: dict object at 0x10131a6e0> :  {'y': 5, 'x': 3, 'z': 0, 'g1': {'funcs': {'sub': <function <lambda> at 0x1012d9398>, 'wut': <function <lambda> at 0x1012d9410>, 'add': <function <lambda> at 0x1012d9320>, 'mul': <function <lambda> at 0x1012d9230>, 'div': <function <lambda> at 0x1012d92a8>, 'wat': <function wat at 0x1012d91b8>}, 'test_global': 5, 'g': {...}, '__builtins__': <module '__builtin__' (built-in)>, 'calculator': <function calculator at 0x1012d9488>, '__package__': None, '__name__': '__main__', 'wat': <function wat at 0x1012d91b8>, '__doc__': None}}
# > <cell at 0x1012d8718: int object at 0x7fe4a0c0c0c8> :  3
# > <cell at 0x1012d8750: int object at 0x7fe4a0c0c098> :  5
# > (3, 5, {'y': 5, 'x': 3, 'z': 0, 'g1': {'funcs': {'sub': <function <lambda> at 0x1012d9398>, 'wut': <function <lambda> at 0x1012d9410>, 'add': <function <lambda> at 0x1012d9320>, 'mul': <function <lambda> at 0x1012d9230>, 'div': <function <lambda> at 0x1012d92a8>, 'wat': <function wat at 0x1012d91b8>}, 'test_global': 5, 'g': {...}, '__builtins__': <module '__builtin__' (built-in)>, 'calculator': <function calculator at 0x1012d9488>, '__package__': None, '__name__': '__main__', 'wat': <function wat at 0x1012d91b8>, '__doc__': None}}, {'funcs': {'sub': <function <lambda> at 0x1012d9398>, 'wut': <function <lambda> at 0x1012d9410>, 'add': <function <lambda> at 0x1012d9320>, 'mul': <function <lambda> at 0x1012d9230>, 'div': <function <lambda> at 0x1012d92a8>, 'wat': <function wat at 0x1012d91b8>}, 'test_global': 5, 'g': {...}, '__builtins__': <module '__builtin__' (built-in)>, 'calculator': <function calculator at 0x1012d9488>, '__package__': None, '__name__': '__main__', 'wat': <function wat at 0x1012d91b8>, '__doc__': None}, {'funcs': {'sub': <function <lambda> at 0x1012d9398>, 'wut': <function <lambda> at 0x1012d9410>, 'add': <function <lambda> at 0x1012d9320>, 'mul': <function <lambda> at 0x1012d9230>, 'div': <function <lambda> at 0x1012d92a8>, 'wat': <function wat at 0x1012d91b8>}, 'test_global': 5, 'g': {...}, '__builtins__': <module '__builtin__' (built-in)>, 'calculator': <function calculator at 0x1012d9488>, '__package__': None, '__name__': '__main__', 'wat': <function wat at 0x1012d91b8>, '__doc__': None})


g = globals()
print 'g' in globals()
print globals()['g'] == g
print globals()['g']['g']['g']['g'] == globals()
print "recursive dictionary fun"

# d = {1: d}                            # <- not allowed
# globals_dict = {g: globals_dict}      # <- allowed?
