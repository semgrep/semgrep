# Integer comparisons that produce True
a = (3 == 3)
b = (3 != 4)
c = (2 < 3)
d = (4 > 3)
e = (3 <= 3)
f = (3 >= 3)

# Boolean comparisons that produce True
g = (True == True)
h = (True != False)

# ERROR:
sink(a)
# ERROR:
sink(b)
# ERROR:
sink(c)
# ERROR:
sink(d)
# ERROR:
sink(e)
# ERROR:
sink(f)
# ERROR:
sink(g)
# ERROR:
sink(h)

# These fold to False and must not match sink(True)
i = (3 == 4)
j = (4 < 3)
sink(i)
sink(j)
