def bar()
    baz = 0
    #ERROR: match
    foo(baz + 42)
    #ERROR: match
    foo(/hello #{world + 42}/)
end
