# ruleid: template-string
t"Hello {name}"

# ruleid: escaped-template
escaped(t"A😀")
# ruleid: escaped-template
escaped(t"\x41\U0001f600")
# ruleid: escaped-template
escaped(t"\u0041\N{GRINNING FACE}")
# ruleid: escaped-template
escaped(t"\101\N{GRINNING FACE}")
# ruleid: escaped-template
escaped(t"\N{LATIN CAPITAL LETTER A}\N{GRINNING FACE}")
# ok: escaped-template
escaped(rt"\x41\U0001f600")
# ok: escaped-template
escaped(rt"\N{value}")
# ruleid: template-expression
raw_named(rt"\N{source()}")
# ruleid: template-string
T'Hello {user}'
# ruleid: template-string
rt"Hello {name}"
# ok: template-string
f"Hello {name}"
# ok: template-string
"Hello {name}"

# ruleid: template-expression
t"{source()}"
# ruleid: template-expression
t"{value:{source()}}"

# ok: plain-string
consume(t"hello")
# ok: plain-string
consume(t"{'hello'}")
# ruleid: plain-string
consume(f"hello")
# ruleid: plain-string
consume("hello")

template = t"hello"
# ok: plain-string
consume(template)
joined = t"hel" t"lo"
# ok: plain-string
consume(joined)

# ok: string-comparison
compare(t"hello")
# ok: string-comparison
compare(template)
# ok: string-comparison
compare(joined)
# ruleid: string-comparison
compare(f"hello")
# ruleid: string-comparison
compare("hello")

# ruleid: cooked-template
newline(t"\n{value}")
# ruleid: cooked-template
newline(t"""
{value}""")
# ok: cooked-template
newline(rt"\n{value}")
# ok: cooked-template
newline(Tr"\n{value}")

# ruleid: any-template
show(t"")
# ruleid: any-template
show(t"Hello {name}, {value}")
# ok: any-template
show(f"Hello {name}")

# ruleid: template-expression
tainted_template = t"{source()}"
# ruleid: template-taint
sink(tainted_template)
# ok: template-taint
sink(t"clean")
