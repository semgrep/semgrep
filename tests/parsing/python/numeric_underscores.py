# PEP 515 underscores as visual separators in numeric literals (Python 3.6).

# Decimal int.
a = 1_000_000
b = 10_000_000_000

# Hexadecimal, octal, binary.
c = 0x_dead_beef
d = 0xDEAD_BEEF
e = 0o_755
f = 0b_1010_1010

# Float with fractional, exponent, and underscores in both parts.
g = 1_000.5
h = 1_000.000_5
i = 1_000.5e1_0
j = 1.5e-1_0

# Complex / imaginary literal.
k = 1_000j
m = 1_000.5j
