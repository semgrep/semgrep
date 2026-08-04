# PEP 646 variadic generics — `*Ts` unpacking in a subscript (Python 3.11).
# ERROR: match
Array = tuple[*Ts]


plain = tuple[int]
