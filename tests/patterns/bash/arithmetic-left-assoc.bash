# '-' is left-associative: (a - b) - c
# MATCH:
echo $(( a - b - c ))

echo $(( a - (b - c) ))
