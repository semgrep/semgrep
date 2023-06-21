#OK:
@Hello := lambda x: x
def f():
    return None

@omega := lambda x: x(x)
def g():
    return None

@omega := x[lambda x: x(x)].hello
def h():
    return None
