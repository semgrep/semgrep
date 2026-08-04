# PEP 526 variable annotations at module and function scope (Python 3.6).
# (field.py already covers class-body annotations.)

# Module-level annotations.
a: int
b: int = 1
c: "list[int]" = []
d: "dict[str, int]" = {"k": 1}

# Augmented annotation with complex target type expression.
e: tuple[int, ...] = (1, 2, 3)  # type: ignore[name-defined]


def f():
    # Function-local annotations.
    x: int
    y: int = 2
    z: "list[str]" = []
    return y, z


# Annotated assignment whose RHS is a complex expression.
g: int = (1 + 2) * 3
h: list = [i for i in range(3)]
