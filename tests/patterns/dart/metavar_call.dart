void foo() {
  foo();

  foo(1);
  foo(2);

  // ERROR:
  foo(1, 2);
  // ERROR:
  bar(1, 2);

  foo(1, 2, 3);
}