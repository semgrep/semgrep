tainted = source()

def foo(x=tainted + "foo"):
    # ruleid: test
    sink(x)

foo()

def bar(x=source()):
    # ruleid: test
    sink(x)

bar()

def baz(x="safe"):
    # ok: test
    sink(x)

baz()
