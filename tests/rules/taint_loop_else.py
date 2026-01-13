def test1():
    x = source
    for _ in range(10):
        pass
    else:
        # ruleid: loop-else
        sink(x)

def test2():
    x = source
    while b():
        pass
    else:
        # ruleid: loop-else
        sink(x)
