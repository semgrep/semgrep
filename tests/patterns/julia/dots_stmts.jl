function foo()
    # ERROR:
    user_data = get()
    println("do stuff")
    foobar()
    eval(user_data)
end
