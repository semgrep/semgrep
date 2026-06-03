a = 12 & 15     # AND:  1100 & 1111 = 1100 = 12
b = 8 | 4       # OR:   1000 | 0100 = 1100 = 12
c = 15 ^ 3      # XOR:  1111 ^ 0011 = 1100 = 12
d = ~(-13)      # NOT:  ~n = -(n+1), so ~(-13) = 12
e = 3 << 2      # LSL:  3 * 4 = 12
f = 48 >> 2     # ASR:  48 / 4 = 12

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
