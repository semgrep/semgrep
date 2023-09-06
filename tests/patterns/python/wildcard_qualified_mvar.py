from B import *

# We don't match this, because we don't unpack wildcard
# imports for patterns with metavariables.
# OK:
foo(x)

# ERROR:
foo(B.x)

foo(y)

foo(B.y)