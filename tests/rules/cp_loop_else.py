def test1():
    a = 6120
    for _ in range(10):
        pass
    else:
        a = 3110
        # ruleid: loop-else
        f(a)

    # ruleid: loop-else
    f(a)

def test2():
    a = 6120
    for _ in range(10):
        break
    else:
        a = 3110
        # ruleid: loop-else
        f(a)

    # ok: loop-else
    f(a)

def test3():
    a = 6120
    for _ in range(10):
        continue
        break
    else:
        a = 3110
        # ruleid: loop-else
        f(a)

    # ruleid: loop-else
    f(a)

def test4():
    a = 6120
    while b():
        pass
    else:
        a = 3110
        # ruleid: loop-else
        f(a)

    # ruleid: loop-else
    f(a)

def test5():
    a = 6120
    while b():
        break
    else:
        a = 3110
        # ruleid: loop-else
        f(a)

    # ok: loop-else
    f(a)

def test6():
    a = 6120
    while b():
        continue
    else:
        a = 3110
        # ruleid: loop-else
        f(a)

    # ruleid: loop-else
    f(a)
