# PEP 695 generic function with a type parameter list (Python 3.12).
# ERROR: match
def first[T](items):
    return items[0]


def plain(items):
    return items[0]
