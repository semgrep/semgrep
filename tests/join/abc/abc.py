def abc(a, b, c):
    z = a + b
    # MATCH:
    z = a + c
    z = b + c
    return z