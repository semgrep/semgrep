def foo()
    # ERROR:
    foo("whatever sequence of chars")
    # foo('whatever sequence of chars')

    # this string is not a constant, and therefore will not be matched.
    foo("string #{var} interpolation")
end

