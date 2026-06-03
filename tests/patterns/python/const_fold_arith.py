a = 2 + 4  # addition
b = 10 - 4  # subtraction
c = 2 * 3  # multiplication

d = 18 // 3  # floor division
d1 = 20 // 3  # floor division

d2 = 18 / 3  # true division
d3 = 18 / 4  # true division with non-integer result
dzero = 18 / 0 


d4 = -22 // 4
d5 = 20 // 0

e = 27 % 7  # modulo (27 = 3*7 + 6)
f = 2**2 + 2  # exponentiation
g = -(4 - 10)  # unary minus
h = +(3 + 3)  # unary plus (identity)

# ERROR:
sink(a)
# ERROR:
sink(b)
# ERROR:
sink(c)

# ERROR:
sink(d)
# ERROR:
sink(d1)
# ERROR:
sink(d2)
# OK:
sink(d3)
# ERROR:
sink(-d4)
# OK:
sink(d5)
# OK: 
sink(dzero)

# ERROR:
sink(e)
# ERROR:
sink(f)
# ERROR:
sink(g)
# ERROR:
sink(h)
