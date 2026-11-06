# ERROR: match
for outer x in xs
    consume(x)
end
for x in xs
    consume(x)
end
for outer in xs
    consume(outer)
end
