# Force Python parsing through tree-sitter with syntax unsupported by pfff,
# while covering every valid spelling of an f-string prefix.
value = 1

a = f"text{value}"
b = F"text{value}"
c = fr"text{value}"
d = fR"text{value}"
e = Fr"text{value}"
f = FR"text{value}"
g = rf"text{value}"
h = rF"text{value}"
i = Rf"text{value}"
j = RF"text{value}"

# Quote style does not change prefix semantics.
single = rf'text{value}'
triple = RF"""text{value}"""

match value:
    case 1:
        print("hit")
    case _:
        pass

type Alias = int

def identity[T](v: T) -> T:
    return v
