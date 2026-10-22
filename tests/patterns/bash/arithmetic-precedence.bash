# MATCH:
echo $(( a * 2 ))

# MATCH:
echo $(( (a + b) * 2 ))

# '*' binds tighter than '+': this is a + (b * 2)
echo $(( a + b * 2 ))

# this is (a * 2) + 1
echo $(( a * 2 + 1 ))
