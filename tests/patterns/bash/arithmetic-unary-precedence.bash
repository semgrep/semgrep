# unary operators bind tighter than binary ones: (-a) + b
# MATCH:
echo $(( -a + b ))

# MATCH:
echo $(( !a + b ))

echo $(( -(a + b) ))
