# PEP 3134 exception chaining `raise X from Y` (Python 3.0).
def f():
    try:
        g()
    except ValueError as e:
        # ERROR: match
        raise RuntimeError("wrapped") from e


def plain():
    raise ValueError("no from clause")
