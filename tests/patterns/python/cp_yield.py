def test(x):
    c = 42
    if x < 0:
        #ERROR:
        yield c
    else:
        yield x
