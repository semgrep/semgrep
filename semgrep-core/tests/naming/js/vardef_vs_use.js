// global
var x = 1;
foo(x);

function foo(a) {
    // local
    var y = 1;

    bar(y);
    bar(x);
    bar(a);
}

function test2() {
  // the addition of the local y in foo should not be visible here
  bar(y)
}
