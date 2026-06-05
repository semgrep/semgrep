# PEP 498 f-strings (Python 3.6).

x = 1
name = "alice"

# Simple substitution.
a = f"x={x}"

# Multiple substitutions.
b = f"x={x}, name={name}"

# Conversion flags.
c = f"{x!r}"
d = f"{x!s}"
e = f"{x!a}"

# Format spec.
g = f"{x:>10}"
h = f"{x:0>5d}"
i = f"{x:.{2}f}"

# Conversion + format spec.
j = f"{x!r:>10}"

# Expression: attribute access, subscript, call.
class C:
    attr = 1
obj = C()
d_map = {"k": 1}

def fn(z):
    return z

k = f"{obj.attr}"
l = f"{d_map['k']}"
m = f"{fn(x)}"

# Arithmetic and conditional expression.
n = f"{x + 1}"
o = f"{x if x > 0 else -x}"

# Triple-quoted, multi-line f-string.
p = f"""
line 1: {x}
line 2: {name}
"""

# Prefixes: f-string + raw, in either order.
q = rf"\n{x}"
r = fr"\n{x}"
s = Rf"\n{x}"
t = fR"\n{x}"

# Empty f-string.
u = f""

# Implicit concatenation: plain + f-string + plain.
v = "prefix=" f"{x}" " suffix"
