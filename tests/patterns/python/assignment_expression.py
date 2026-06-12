# PEP 572 assignment expressions — walrus `:=` (Python 3.8).
def f(xs):
    # ERROR: match
    if (n := len(xs)) > 3:
        return n
