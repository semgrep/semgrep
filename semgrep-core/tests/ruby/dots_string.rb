def foo()
    # ERROR:
    foo("whatever sequence of chars")
    # foo('whatever sequence of chars')
    foo("string ${var} interpolation")
end

