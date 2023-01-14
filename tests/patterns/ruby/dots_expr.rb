def foo()
    # ERROR:
    bar = foo(1, 2, 3, 4, 5)
    # ERROR:
    baz = foo(5) + 1
end

