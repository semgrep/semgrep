# None of these will match, because patterns with metavariables
# do not activate wildcard imports.
# OK:
bar(
  # OK:
  z
)

from A import *

# OK:
foo(
  # OK:
  x,
  # OK:
  y
)