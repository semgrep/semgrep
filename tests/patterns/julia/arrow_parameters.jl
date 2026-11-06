# ERROR: match
(x, y) -> consume(x)
# ERROR: match
(a, b, c) -> consume(a)
(x, y) -> consume(y)
(x, y) -> other(x)
