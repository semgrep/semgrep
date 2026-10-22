# MATCH:
until [[ "$x" = 42 ]]; do
  echo "$x"
done

while [[ "$x" = 42 ]]; do
  echo "$x"
done
