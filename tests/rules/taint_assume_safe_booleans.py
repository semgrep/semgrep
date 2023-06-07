def ok():
    x = "tainted"
    y = x == "safe"
    #OK: test
    sink(y)

def bad():
    x = "tainted"
    y = x or "safe"
    #ruleid: test
    sink(y)
