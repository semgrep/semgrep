function foo()
    # ERROR:
    foo("whatever sequence of chars")
    # foo('whatever sequence of chars')

    # this string is not a constant, and therefore will not be matched.
    foo("$foo, here")

    # this string is not a constant, and therefore will not be matched.
    foo("$(1 + 2) = 3")
end
