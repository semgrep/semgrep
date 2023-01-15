# MATCH:
for (( c=1; c<=5; c++ ))
do
  echo $c
done

# MATCH:
for (( c=1; c<=5; c++ )) {
  echo $c
}

# MATCH:
for (( ; ; ))
do
  echo 'forever'
done
