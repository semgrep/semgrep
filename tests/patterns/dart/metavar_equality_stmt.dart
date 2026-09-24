void main() {
  var x = 1;
  // ERROR:
  if (x > 2)
    foo();
  else
    foo();

  if (x > 2)
    foo();
  else
    bar();
}
