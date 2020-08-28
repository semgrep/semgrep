f(x := 2)
y = (x := 3)

# Valid alternative
(x := 0)

# Valid alternative
x = (y := 0)

# Valid
len(lines := f.readlines())

# Valid
foo(x := 3, cat='vector')

# Valid alternative
foo(cat=(category := 'vector'))

# Valid
partial_sums = [total := total + v for v in values]

# Valid
while x := next(f):
    print(x)
