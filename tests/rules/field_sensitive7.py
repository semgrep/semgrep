x = safe

# `x.a` matches a taint source!
#ruleid: test
sink(x.a.b)

# `x.a` has been tainted by side-effect...
#ruleid: test
sink(x.a)
