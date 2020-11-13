def func1():
    # ruleid:open-never-closed
    fd = open('foo')
    x = 123


def func2():
    # ok
    fd = open('bar')
    fd.close()

def func3():
    # ok
    fd = open('baz')
    try:
        pass
    finally:
        fd.close()
