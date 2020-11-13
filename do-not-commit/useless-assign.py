# ruleid: useless-assignment
x = 5
x = 5

def foo():
    pass

# ruleid: useless-assignment
x = 5
x = 6

def foobar():
    pass

# this should be ok, or at least a different rule id, since xz might be global and used in y()
xz = y
xz = y()

y() = y()

# todo, this should be ok
# ruleid: useless-assignment
x1 = 1
x1 = x1 + 1

# OK
z = '1'
z = z.rstrip('1')

# OK
aa = 'hi'
aa = some_func(aa)
