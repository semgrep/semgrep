def foo(x=42):
    print(x)

# No autofix
foo()
foo(
)
foo(43)
foo(
43
)

# Autofix of a partial line
foo(42)
foo(
42)
foo(42
)

# Autofix of an entire line
foo(
42
)
