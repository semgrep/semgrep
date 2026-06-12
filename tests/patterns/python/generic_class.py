# PEP 695 generic class with a type parameter list (Python 3.12).
# ERROR: match
class Box[T]:
    def __init__(self, value):
        self.value = value


class Plain:
    pass
