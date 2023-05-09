def abc(a, b, c, d, p):
    # MATCH:
    z = a + c + 10
    z = b + c + 11
    z = c + d + 12
    # MATCH: this is a little odd, maybe should not match
    z = a + b + 13
    # MATCH:
    z = p + c + 14
    z = p + q + 15
    return z