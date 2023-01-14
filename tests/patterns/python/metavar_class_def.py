# ERROR:
class Foo:
    def foo ():
        foo (3)
# ERROR:
class Anything(Foo):
    def foos ():
        foo (4)

def foo(var):
    foo (3)


