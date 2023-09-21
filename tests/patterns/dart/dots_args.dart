void foo(int x, int y) {
  foo();

  // ERROR:
  foo(5);

  // ERROR:
  foo(1, 5);

  bar(5);
}