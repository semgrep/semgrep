function foo(input) {
  var x;
  if (random()) {
    x = "hello";
  }
  else {
    throw "error";
    x = input; // unreachable
  }
  //ERROR:
  return x;
}
