# MATCH:
baz() {
  foo "$(echo 'hello' | bar)"
}
