def foo()
    #ERROR: match
    foo()
    bar()
    #ERROR: match
    foo()
    x = bar()
    #ERROR: match
    foo()
    print(bar())
    #ERROR: match
    foo()
    return bar()
end
