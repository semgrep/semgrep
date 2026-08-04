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


# --- Intraprocedural (IL) constant folding: Eval_il_partial ---
# Reassigning each local forces the flow-sensitive IL svalue analysis (rather
# than the syntactic generic pass) to do the folding. Each still folds to 12.
def _il_bitwise():
    a = 0
    a = 12 & 15  # AND
    # ERROR:
    sink(a)
    b = 0
    b = 8 | 4  # OR
    # ERROR:
    sink(b)
    c = 0
    c = 15 ^ 3  # XOR
    # ERROR:
    sink(c)
    d = 0
    d = ~(-13)  # NOT
    # ERROR:
    sink(d)
    e = 0
    e = 3 << 2  # LSL
    # ERROR:
    sink(e)
    f = 0
    f = 48 >> 2  # ASR
    # ERROR:
    sink(f)
