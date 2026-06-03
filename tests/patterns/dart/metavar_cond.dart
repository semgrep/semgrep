void main() {
  var x = 1;
  // ERROR:
  if (x > 2)
    foo();

  if (x == 1)
    bar();

  if (x > 2) {
    baz();
  }
}
