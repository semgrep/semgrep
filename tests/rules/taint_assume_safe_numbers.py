def ok(x):
    y = x + 1
    #OK: test
    sink(y)

def bad(x):
    y = x + "something"
    #ruleid: test
    sink(y)
