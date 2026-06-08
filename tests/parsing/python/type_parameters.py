# PEP 695 type-parameter syntax (Python 3.12).

# Type alias statement.
type Point = tuple[float, float]

# Generic function.
def first[T](items: list[T]) -> T:
    return items[0]

# Generic class.
class Box[T]:
    def __init__(self, value: T) -> None:
        self.value = value

    def get(self) -> T:
        return self.value
