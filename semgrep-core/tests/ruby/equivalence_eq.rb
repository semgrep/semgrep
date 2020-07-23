def foo()
    a = 1
    b = 2
    # ERROR:
    if a + b == a + b
        foo()
    end
end
