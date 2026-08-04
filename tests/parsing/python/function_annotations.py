# PEP 3107 function annotations: parameters and return values (Python 3.0).


# Return annotation only.
def r() -> int:
    return 0


# Single annotated parameter.
def p(x: int):
    return x


# Annotated parameters + return.
def pr(x: int, y: str) -> bool:
    return bool(x) and bool(y)


# Annotation combined with a default value.
def default(x: int = 0, y: str = "s") -> None:
    return None


# Annotations on *args / **kwargs.
def varargs(*args: int, **kwargs: str) -> None:
    return None


# Annotation with a keyword-only parameter (bare `*` separator).
def kwonly(a: int, *, b: str = "b") -> int:
    return a


# Annotations whose types are arbitrary expressions: subscripts, attributes,
# string forward references, and a callable type.
def complex_types(
    items: "list[int]",
    mapping: dict[str, int],
    cb: "Callable[[int], int]",
) -> "tuple[int, ...]":
    return ()


# Lambdas cannot be annotated, but a lambda may appear as a default value
# alongside an annotated parameter.
def with_lambda_default(f: object = lambda x: x) -> object:
    return f


# Method parameters and return values inside a class.
class C:
    def m(self, x: int) -> "C":
        return self

    @staticmethod
    def s(x: int) -> int:
        return x


# Nested function carrying its own annotations.
def outer(a: int) -> int:
    def inner(b: int) -> int:
        return a + b

    return inner(a)
