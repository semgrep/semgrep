# MATCH:
foo(bar=True), foo(bar=True)

foo(bar=False)

foo(baz=True)

# MATCH:
foo(  bar   =    True  )

# MATCH:
foo(x, bar=True)
# MATCH:
foo(x, bar=True, baz=True)
