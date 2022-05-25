# ok: test
class A:
    foo
    bar

# ok: test
class B:
    if TYPE_CHECKING:
        foo
    bar

# ruleid: test
class C: pass

# ruleid: test
class D:
    def foo():
        x = 1

    pass
