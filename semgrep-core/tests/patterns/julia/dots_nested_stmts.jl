function foo()
    #ERROR: match
    if x == some_cond
        println("matched")
    else
        println("not matched")
    end
end