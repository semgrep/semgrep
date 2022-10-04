
var x = 2

var y = g(x)

var x = 3

function foo() {
  // No finding here, because `x` is not known to be assigned to once!
  var z = f(y)
}