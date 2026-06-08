# PEP 570 positional-only parameters `/` (Python 3.8).


def basic(a, b, /, c, d):
    return a + b + c + d


def with_defaults(a, /, b=1, *, c=2):
    return a + b + c


def only_positional(x, y, /):
    return x * y


def mixed(a, b, /, c, *args, d, **kwargs):
    return (a, b, c, args, d, kwargs)
