# '**' is right-associative: 2 ** (3 ** 2)
# MATCH:
echo $(( 2 ** 3 ** 2 ))

echo $(( (2 ** 3) ** 2 ))
