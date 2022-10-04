f() {
  # MATCH:
  echo "what's up?"
  return 1
}

# MATCH:
function gg() { ls; echo hello; }

h() { nothing; }
