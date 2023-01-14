def foo
    # ERROR:
    if 1 != 3 then
        foo()
    end

    # ERROR:
    if 1 != 3
        foo()
    end

    # ERROR:
    if 1 == 1
        foo()
        bar()
        baz()
    end
end

