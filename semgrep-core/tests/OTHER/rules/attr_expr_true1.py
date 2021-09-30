# https://github.com/returntocorp/semgrep/issues/3489

from foo.bar import baz

#ruleid:test
baz.func("HAI")

#ruleid:test
@baz.func("HAI")
def f():
    pass
