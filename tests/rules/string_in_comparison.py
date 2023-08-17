
# ruleid: string-in-comparison
foo(a)
# ruleid: string-in-comparison
foo(b)
# ruleid: string-in-comparison
foo(c)
# ruleid: string-in-comparison
foo(ab)
# ruleid: string-in-comparison
foo(bc)
# ruleid: string-in-comparison
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

# ruleid: string-in-comparison
bar(ac)
# ruleid: string-in-comparison
bar(abcd)
# ruleid: string-in-comparison
bar(x)
# ruleid: string-in-comparison
bar(xyz)

# this shouldn't parse as a regexp, it should just be a literal string
# ok: string-in-comparison
baz(".*")

# this one's OK though
# ruleid: string-in-comparison
qux(".*")