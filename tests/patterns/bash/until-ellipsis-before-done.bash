# MATCH:
until true; do echo a; done

# MATCH:
until false; do
  echo a
  echo b
done

# OK:
while true; do echo a; done
