
# ERROR: match
foo(a)
# ERROR: match
foo(b)
# ERROR: match
foo(c)
# ERROR: match
foo(ab)
# ERROR: match
foo(bc)
# ERROR: match
foo(abc)

foo()
foo(ac)
foo(abcd)

bar()
bar(a)
bar(b)
bar(c)
bar(ab)
bar(bc)
bar(abc)

# ERROR: match
bar(ac)
# ERROR: match
bar(abcd)
# ERROR: match
bar(x)
# ERROR: match
bar(xyz)