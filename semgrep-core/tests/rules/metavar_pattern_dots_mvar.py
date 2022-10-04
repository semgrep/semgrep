# ruleid:test-mvp-dots-mvar
def f():
    foo(1)
    bar(2)
    baz(3)

# ruleid:test-mvp-dots-mvar
def g():
    foo(3)
    meh(2)
    baz(1)

# OK:test-mvp-dots-mvar
def h():
    baz(1)
    bar(2)
    foo(3)
