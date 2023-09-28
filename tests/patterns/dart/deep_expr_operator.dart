void foo(int x, int y) {
  foo();

  foo(2);

  foo(1, 2, 3);

  // ERROR:
  foo(42);

  // ERROR:
  foo(bar(42));
}