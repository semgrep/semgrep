function foo(input) {
  var x = "hello";
  if (random()) {
    bar();
  }
  else {
    throw "error";
    x = input; // unreachable
  }
  //ERROR:
  return x;
}
