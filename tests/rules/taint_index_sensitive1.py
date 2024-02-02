x[1] = safe
x[42] = source()
#ok: test
sink(x)
#ok: test
sink(x[1])
#ruleid: test
sink(x[42])
#ruleid: test
sink(x[i])
