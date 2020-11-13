class A:

    def __init__(a, b, c):
        # ruleid:return-in-init
        return A(a, b, c)

class A:

    def __init__(a, b, c):
        # ruleid:return-in-init
        return


class B:

    def __init__(a, b, c):
        # todoruleid:yield-in-init
        yield

class C:

    def __init__():
        # todoruleid:yield-in-init
        yield 5


def __init__(a, b, c):
    # OK, not in class
    return A(a, b, c)


def __init__(a, b, c):
    # OK, not in class
    yield

def __init__():
    # OK, not in class
    yield 5
