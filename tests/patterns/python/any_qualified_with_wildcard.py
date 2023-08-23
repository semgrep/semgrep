# will match because wildcard imports in matching are acontextual, unfortunately
# ERROR:
bar(
  # ERROR:
  z
)

from A import *

# ERROR:
foo(
  # ERROR:
  x,
  # ERROR:
  y
)