#ERROR:
a = "foo"

#ERROR:
b = a

#ERROR:
c[a] = 1

#ERROR:
d = {1: a}

def f():
    #ERROR:
    return a
