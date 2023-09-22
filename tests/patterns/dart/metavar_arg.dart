void foo(int x, int y) {
  foo();

  foo(2);

  // ERROR:
  foo(1, 2);

  foo(1, 2, 3);
}