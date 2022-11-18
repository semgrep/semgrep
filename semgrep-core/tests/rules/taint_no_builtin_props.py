
def bad():
    x = "password"
    y = x
    z = y
    #ruleid: secrets
    sink(z)

def ok():
    x = "password"
    y = x
    z = y+"ok"
    #ok: secrets
    sink(z)
