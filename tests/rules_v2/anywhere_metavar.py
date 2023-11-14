def foo():
    # ruleid: du-same-file
    foo()
    # ruleid: du-same-file
    bar()
    g()
    baz()


def bar():
    quux()
    # ruleid: du-same-file
    bar()
    f()
    # ruleid: du-same-file
    foo()

