# MATCH:
if true; then echo a; fi

# MATCH:
if [ -f x ]; then
  echo a
  echo b
fi

# MATCH:
if true; then
  echo a
else
  echo b
fi

# OK:
while true; do echo a; done
