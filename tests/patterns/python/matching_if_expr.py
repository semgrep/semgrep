x = 5

# MATCH:
if x == 5:
    x += 1
else:
    x += 3

# MATCH:
if isinstance(x, int):
    print(f"{x} is a number-like object")
