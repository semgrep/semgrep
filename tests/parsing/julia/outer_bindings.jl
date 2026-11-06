function reuse(xs)
    x = 0
    for outer x in xs
        consume(x)
    end
    for outer in xs
        consume(outer)
    end
    [x for outer x in xs if x > 0]
end
