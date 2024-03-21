def foo(x):
    sink(x)

def bar():
    y = "taint"
    foo(y)
