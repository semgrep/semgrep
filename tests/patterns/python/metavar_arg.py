def foo():
    # ERROR:
    foo(1, 2)

    # ERROR:
    foo(a_very_long_constant_name, 2)

    # ERROR:
    foo(unsafe(), 2)  # indeed

    # ERROR:
    foo(bar(1, 3), 2)

    foo(2, 1)
