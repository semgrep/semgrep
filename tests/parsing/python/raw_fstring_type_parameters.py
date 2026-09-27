# Regression test for #11935: a raw f-string containing a backslash and an
# interpolation must not poison parsing of a following PEP 695 type parameter.

X = rf"\b{1}"

def f[T: int](x: T) -> T:
    return x
