def foo():
    # ERROR:
    foo("whatever sequence of chars")
    # foo('whatever sequence of chars')
    foo(f'string {var} interpolation')
    # ERROR:
    foo(f'constant string')
