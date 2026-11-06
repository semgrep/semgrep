f(x, y=2; z=3)
f.(x; z=3)
named = (; x=1, y=2)
singleton = (x=1,)
sequence = (x=1;; x + 2;)
generator = (x * y for x in xs for y in ys if y > 0)
result = sum(x * y for x in xs, y in ys if x > 0)
tuples = (1, (x for x in xs), 3)
array = [x * y for x in xs for y in ys if y > 0]
matrix = [a b; c d]
map(xs) do x::Int, rest...
    consume(x, rest...)
end
arrow = (x, y) -> (x = y)
let x::Int = 1, y = 2
    consume(x, y)
end
