package Foo

func foo() {
  //ERROR: match
  foo()
  bar()
  foobar()
  // this should not matter! but it was because of some hidden Empty
  // originally added by the Go parser.
  otherstuff()
}