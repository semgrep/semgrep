# PEP 3132 extended iterable unpacking `a, *b = xs` (Python 3.0).
xs = [1, 2, 3, 4]
# ERROR: match
first, *rest = xs


a, b = (1, 2)
