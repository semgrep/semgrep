def f1(x: tuple[int]):
    pass

def f2() -> tuple[str | None]:
    ...

x: tuple[float] | None = None

y: list[tuple[int]] = []

z = tuple[int]

#ERROR: match
a: tuple[int, ...] = 1
#ERROR: match
b: tuple[str | None, ...]
#ERROR: match
c: tuple[int, str] = (1, "2")
