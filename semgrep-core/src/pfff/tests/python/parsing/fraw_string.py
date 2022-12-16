
# raw string seems to require the \ to be followed
# by a char, which is good given how our python lexer
# is written.
# However, in fr'\{{',the first { is considered
# escaped currently by our lexer, which then does not
# give a chance for the lexer to see the {{

a = 1
s = fr'foo{a}\{{\n}}'

print(s)
