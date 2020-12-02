# ERROR:
def foo():
    foo(1,2)

# ERROR:
def bar(bar1, bar2, bar3):
    bar (1,2,3)

# ERROR:
def foobar(bar1) -> int:
    bar(1,2,3)
    foo()
    return 3
