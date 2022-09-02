
var x = 2

var y = g(x)

// ruleid: find-propagated-non-constant-exp 
var h = f(y)

x = 3

function foo() {
  // ruleid: find-propagated-non-constant-exp 
  var z = f(y)
}