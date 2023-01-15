baz() {
  # MATCH:
  foo bar

  foo bar baz

  # MATCH:
  foo "$(echo 'hello' | bar)"
}
