def foo()
    # ERROR:
    if x > 2
        foo()
        bar()
    else
        foo()
        bar()
    end

    if x > 2
        foo()
        bar()
    else
        foo()
    end
end

