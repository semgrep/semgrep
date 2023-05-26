def foo():
    # ruleid: test
    foo.Bar.alice = 2 * my_module.UNIT

def bar():
    x = foo.Bar
    # ruleid: test
    x.alice = 3 * my_module.UNIT

def alice():
    x = foo.Bar
    y = x
    # ruleid: test
    y.alice = 4 * my_module.UNIT
