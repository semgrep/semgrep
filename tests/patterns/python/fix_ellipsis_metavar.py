# MATCH:
foo(bar=True), foo(bar=True)

foo(bar=False)

foo(baz=True)

# MATCH:
foo(bar=True)

# TODO These should work, but end up with overlapping fixes. See
# https://github.com/returntocorp/semgrep/issues/4963

# foo(x, bar=True)
# foo(x, bar=True, y)
