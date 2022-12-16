
# weirdly this is not accepted
#s = r'\'

# neither is this
#s = r"\"

# neither is this
#s = fr'foo\'

# so we can assume an antislash in a raw string
# must be followed by something other than the ending char

s = r'aa\n'
print(s)

s2 = 'aa\n'
print(s2)
