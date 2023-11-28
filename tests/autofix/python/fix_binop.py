# MATCH:
foo(3)
# MATCH:
foo(5 + 3)
# Test case for #2902
# MATCH:
foo(1 + (5 + 3))
