# MATCH:
while true; do echo a; done

# MATCH:
while read x; do
  echo a
  echo b
done

# MATCH:
while true; do
  echo a
done > /dev/null

# OK:
if true; then
  echo a
fi

# OK:
for x in a b; do echo a; done
