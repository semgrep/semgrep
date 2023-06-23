def foo() do
    #ERROR:
    foo("whatever sequence of chars")
    #actually charlist are quite different from strings in Elixir, so no match here
    foo('whatever sequence of chars')
end