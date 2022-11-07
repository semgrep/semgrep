x[i] = tainted
# something got tainted, let's assume array is tainted
#ruleid: test
sink(x[j])

# something got clean, let's assume array is clean
x[j] = safe
#ok: test
sink(x[k])
