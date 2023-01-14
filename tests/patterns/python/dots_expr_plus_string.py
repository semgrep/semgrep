FOOBAR = 42
bar = 'barbar'
#ERROR: match
foo = bar + "baz" + f"{FOOBAR}FOO"
#ERROR: match
foo2 = bar + "baz" + f"{FOOBAR}FOO" + bar

print(42)
