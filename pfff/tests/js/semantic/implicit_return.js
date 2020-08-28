// See https://jaketrent.com/post/javascript-arrow-function-return-rules/
// for more context.

function foo() {
  return  1;
}

function bar() {
    // there is no implicit return in Javascript (except for arrows)
    // this will "return" undefined actually.
    foo();
}

// there is an implicit return only for arrow with a single expression
const arrow = () => 1;


console.log(foo());
console.log(bar());
