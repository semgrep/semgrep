# PEP 614 / f-string self-documenting `=` (`f"{x=}"`) (Python 3.8).

x = 42
name = "alice"
point = (1, 2)

# Basic debug form.
a = f"{x=}"

# Debug form with format spec.
b = f"{x=:>5}"

# Multiple debug substitutions.
c = f"{x=}, {name=}"

# Debug on attribute and subscript expressions.
class C:
    attr = 1


obj = C()
d = f"{obj.attr=}"
e = f"{point[0]=}"
