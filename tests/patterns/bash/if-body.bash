# MATCH:
echo hello

if true; then
  # MATCH:
  echo bye
fi
