void foo() {
  // ERROR:
  if (3 > 2) {
    foo();
  }

  if (3 > 2) {
  }

  int x = 2;
}