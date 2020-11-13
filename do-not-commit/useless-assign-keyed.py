d = {}
z = {}
a = {}
for i in xrange(100):
    # ruleid: useless-assignment-keyed
    d[i] = z[i]
    d[i] = z[i]
    d[i+1] = z[i]

    for i in xrange(100):
        # ruleid: useless-assignment-keyed
        da[i*1][j] = z[i]
        da[i*1][j] = z[i]
        da[i*4] = z[i]

# ok for this rule
x = 5
x = 5

x = y
x = y()

y() = y()
