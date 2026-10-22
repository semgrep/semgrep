# Only literals that mean the same in bash and in the generic AST become
# integers: decimal and hexadecimal. Octal '010' (8), bases, and invalid
# forms stay strings.
echo $(( 0 + 42 + 0x1F ))
echo $(( 010 + 2#101 + 1_000 ))
