#OK:
@Hello := lambda x: x
def f():
    return None

#ERROR:
@omega := lambda x: x(x)
def g():
    return None

#ERROR:
@omega := x[lambda x: x(x)].hello
def h():
    return None
