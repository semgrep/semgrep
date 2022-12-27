# ok: pattern-inside-taint (before taint)
foo.qux()
if True:
    foo = provider.foo()
    # ruleid: pattern-inside-taint
    foo.qux()

# ok: pattern-inside-taint (different object)
baz.qux()
# ruleid: pattern-inside-taint
foo.qux()
foo.bar()
# ok: pattern-inside-taint (after taint)
foo.qux()


foo2 = provider.foo()
# ok: pattern-inside-taint (different object)
foo.qux()
# ruleid: pattern-inside-taint
foo2.qux()
foo2.bar()
# ok: pattern-inside-taint (after taint)
foo2.qux()
