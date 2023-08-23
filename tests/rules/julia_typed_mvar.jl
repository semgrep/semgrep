
x :: Int64 = 2
# ruleid: julia-typed-mvar
foo(x)

# no match because not Int64
y :: String = "hi"
foo(y)

# no match because no explicit type annotation
z = 3
foo(z)
