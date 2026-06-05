# PEP 448 generalized unpacking (Python 3.5) in literals and call sites.

a = [1, 2]
b = [3, 4]
d1 = {"x": 1}
d2 = {"y": 2}

# List literal with star unpacking.
l1 = [*a, *b]
l2 = [0, *a, 5, *b, 6]

# Tuple literal with star unpacking.
t1 = (*a, *b)
t2 = (0, *a, 5)

# Set literal with star unpacking.
s1 = {*a, *b}
s2 = {0, *a}

# Dict literal with double-star unpacking and extra keys.
m1 = {**d1, **d2}
m2 = {"head": 0, **d1, "mid": 1, **d2, "tail": 2}

# Call site: multiple star and double-star unpackings, mixed with positional and keyword.
def f(*args, **kwargs):
    pass

f(*a, *b)
f(0, *a, 1, *b, 2)
f(**d1, **d2)
f(0, *a, k=1, **d1, **d2)
