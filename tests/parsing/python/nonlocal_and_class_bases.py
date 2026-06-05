# `nonlocal` declarations (Python 3.0) and generalized class bases
# with star/double-star unpacking (Python 3.5 PEP 448) and metaclass kwarg.


def outer():
    x = 1

    def inner():
        nonlocal x
        x += 1

    def inner2():
        nonlocal x, x_unused  # nonlocal can declare multiple names
        x = 0

    x_unused = 0
    inner()
    inner2()
    return x


class Meta(type):
    pass


class A:
    pass


class B:
    pass


bases = (A, B)
kwargs = {"key": "value"}


# Basic single base.
class C0(A):
    pass


# Multiple bases.
class C1(A, B):
    pass


# Base with metaclass kwarg.
class C2(A, metaclass=Meta):
    pass


# Multiple bases + metaclass + arbitrary keyword.
class C3(A, B, metaclass=Meta, key="value"):
    pass


# Star unpacking in class bases (PEP 448, 3.5).
class C4(*bases):
    pass


# Star + named kwargs together.
class C5(*bases, metaclass=Meta):
    pass


# Double-star unpacking in class kwargs.
class C6(A, **kwargs):
    pass
