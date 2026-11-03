# PEP 750 template strings (Python 3.14).
name = "world"
value = 42
simple = t"Hello {name}"
upper = T'{value}'
raw = rt"\n{value}"
raw_last = Tr'\n{value}'
empty = t""
literal = t"{{escaped braces}}"
conversions = t"{value!r} {value!s} {value!a}"
combined = t"{name} has {value} items"
format_spec = t"{value:>10} {value:.{precision}f}"
debug = t"{value=}"
expression = t"{obj.attr} {items[0]} {fn(value)} {value + 1}"
nested = t"{t'{value}'}"
concatenated = t"{first}" t"{second}"
multiline = t"""first {first}
second {second}"""
triple_single = t'''value {value}'''
