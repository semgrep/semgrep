
def bad():
    x = source()
    y = x
    z = y
    #ruleid: test
    sink(z)

def ok1():
    x = source()
    y = x.a
    #ok: test
    sink(y)

def ok2():
    x = source()
    y = x()
    #ok: test
    sink(y)
