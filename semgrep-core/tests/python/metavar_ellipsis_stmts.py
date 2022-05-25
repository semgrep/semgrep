# ERROR: match
def foo(x):
    y = bar(x)
    print(y)
    if y:
        return 0
    else:
        return 1 + foo(1)
