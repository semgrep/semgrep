def ok1():
    x = "tainted"
    y = x == "safe"
    #OK: test
    sink(y)

def ok2():
    x = "tainted"
    #OK: test
    sink(x == "safe")

def ok3():
    x = "tainted"
    y = "something " + x
    #OK: test
    sink(x != "safe")

def bad():
    x = "tainted"
    y = x or "safe"
    #ruleid: test
    sink(y)
