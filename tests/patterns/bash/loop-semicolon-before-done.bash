# MATCH:
while true; do echo a; done

# MATCH:
while true; do
  echo a
done

until true; do echo a; done
