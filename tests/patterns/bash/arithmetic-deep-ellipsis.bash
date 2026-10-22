# MATCH:
echo $(( count ))

# deep inside the operator tree
# MATCH:
echo $(( (count + 1) * 2 ))

# a bare name and '$name' denote the same variable
# MATCH:
echo $(( $count - 1 ))

echo $(( total ))

# 'count' here is a command, not the variable
echo $(( $(count) ))
