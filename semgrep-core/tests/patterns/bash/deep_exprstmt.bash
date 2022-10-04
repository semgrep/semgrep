foo() {
  # ERROR: match
  foo
  bar
  # ERROR: not sure this should match really
  foo
  x=$(bar)
  # ERROR: match
  foo
  foo2 "$(bar)"
  # ERROR: match, but not sure it should match really
  foo
  bloo | bar > /dev/null
}
