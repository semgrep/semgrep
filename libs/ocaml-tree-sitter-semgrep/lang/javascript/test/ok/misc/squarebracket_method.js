/*
  Check support for computed method definitions.

  Those are method definitions where the method is in square
  brackets, meaning its name its taken from the variable in brackets.

  https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Method_definitions
*/
var f = "hello";

class K {
  [f]() { console.log("yo"); }
}

var obj = new K();
obj.hello();
