x = source()
x.a = safe
# `x` is tainted by `x.a` is marked clean

# The actual sink is `x` which is tainted, even if `x.a` is not!
#ruleid: test
exotic(x.a)

#ok: test
sink(x.a)
