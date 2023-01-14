def foo() do
    #ERROR:
    foo("whatever sequence of chars")
    #ERROR:
    foo('whatever sequence of chars')
end
