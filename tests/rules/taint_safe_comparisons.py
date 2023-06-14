x = "tainted"
#ruleid: tainting
sink(x)
y = x != "something"
#ok: tainting
sink(y)
