void main() {
  var a = 1;
  var b = 2;
  // ERROR:
  if ((a + b) == (a + b))
    foo();

  // ERROR:
  if (a == a)
    bar();

  if (a == b)
    baz();
}
