# PEP 572 assignment expressions — walrus operator `:=` (Python 3.8).

xs = [1, 2, 3, 4, 5]

# Basic binding in expression.
if (n := len(xs)) > 3:
    print(n)

# Walrus in comprehension filter.
evens = [x for x in xs if (y := x % 2) == 0]

# Walrus in while condition.
i = 0
while (chunk := xs[i : i + 2]):
    i += len(chunk)

# Parenthesized walrus in function call.
print((value := xs[0]) + 1)
