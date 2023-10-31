def foo():
    f = open()
    f.write("bar")
    f.close()
    # ruleid: test
    f.close()
    # the following has no effect on the double-closing of `f`
    # that already happened
    f = open()
    f.write("baz")
    f.close()

def bar():
    f = open()
    f.write("bar")
    f.close()
    # we reopen `f` so it's fine to close it again
    f = open()
    # ok: test
    f.close()
