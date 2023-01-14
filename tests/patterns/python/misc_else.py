def foo():
    # ERROR: match
    if True:
        pass
    else:
        pass


def foo():
    # this should not match
    if True:
        pass
