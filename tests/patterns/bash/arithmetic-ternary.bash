# MATCH:
echo $(( x > 0 ? 1 : 0 ))

# the condition is the whole 'a || b'
# MATCH:
echo $(( a || b ? 1 : 0 ))

echo $(( x > 0 ? 0 : 1 ))

# the ternary is right-associative: x ? 2 : (y ? 1 : 0)
echo $(( x ? 2 : y ? 1 : 0 ))
