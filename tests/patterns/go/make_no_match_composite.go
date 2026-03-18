func f() {
  // ERROR:
  d := make([]string, 1)
  // OK: slice literal should not match make() pattern
  a := []string{"test"}
  // OK: empty slice literal should not match make() pattern
  b := []string{}
  // OK: multi-element slice literal should not match make() pattern
  c := []string{"test", "blah"}
}
