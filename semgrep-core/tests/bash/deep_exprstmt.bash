foo() {
  # ERROR: match
  bar
  # ERROR: match
  x=$(bar)
  # ERROR: match
  foo2 "$(bar)"
  # ERROR: match
  bloo | bar > /dev/null
}
