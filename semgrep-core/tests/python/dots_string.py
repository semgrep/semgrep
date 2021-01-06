def foo():
    # ERROR:
    foo("whatever sequence of chars")
    # foo('whatever sequence of chars')

    # ERROR:
    foo(f'constant string')

    # this string is not a constant, and therefore will not be matched.
    foo(f'string {var} interpolation')
