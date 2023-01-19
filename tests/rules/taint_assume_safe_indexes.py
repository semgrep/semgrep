a = safe()
i = tainted()
x = a[i]
#ok:tainted
sink(x)
