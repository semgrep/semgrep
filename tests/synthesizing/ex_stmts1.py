from execme import dangerous


def foo(arg):
    source()
    print("abc")
    dangerous(1, arg)
