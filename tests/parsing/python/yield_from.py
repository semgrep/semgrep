# PEP 380 `yield from` (Python 3.3).


def inner():
    yield 1
    yield 2
    return "done"


def outer():
    yield from inner()


def outer_with_value():
    # `yield from` is also an expression whose value is the inner's return.
    result = yield from inner()
    yield result


def outer_chain():
    yield from inner()
    yield from inner()


def outer_with_iterable():
    yield from [1, 2, 3]
    yield from (x for x in range(3))
