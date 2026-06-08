# PEP 750 template strings — t-strings (Python 3.14).
#
# NOT YET SUPPORTED by the bundled tree-sitter-python grammar (0.26.3). Each `t"..."` below
# currently parses to an ERROR node.

name = "world"
value = 42

# Basic t-string with an interpolation.
greeting = t"Hello {name}"

# Format spec and conversion.
padded = t"{value:>5}"
debug = t"{value!r}"

# Multiple interpolations.
combined = t"{name} has {value} items"
