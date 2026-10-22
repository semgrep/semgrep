# MATCH:
for x in a b; do echo a; done

# MATCH:
for x in 1 2 3; do
  echo $x
  echo done
done

# OK:
for y in a b; do echo a; done

# OK:
while true; do echo a; done
