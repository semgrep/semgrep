PASSWORD = "password"
def foo()
    #ERROR: match
    func(PASSWORD)
end

# we even handle this case!
A=PASSWORD
def foo2()
    #ERROR: match
    func(A)
end
