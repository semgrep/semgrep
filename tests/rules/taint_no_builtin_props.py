
def bad():
    x = "password"
    y = x
    z = y
    #ruleid: secrets
    sink(z)

def ok1():
    x = "password"
    y = x
    z = y+"ok"
    #ok: secrets
    sink(z)

def ok2():
    x = "password"
    y = x
    z = f(y)
    #ok: secrets
    sink(z)

def ok3():
    x = "password"
    y = x
    z = a[y]
    #ok: secrets
    sink(z)
