package Foo

// global
var x = 1

func foo(a int) {
  //local
  var y = 1

  bar(y)
  bar(x)
  bar(a)

}

func test2() {
 // the addition of the local y in foo should not be visible here
 bar(y)
}
