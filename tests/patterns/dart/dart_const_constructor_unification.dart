void main() {
  // ERROR:
  Foo(1);
  // ERROR:
  Foo(1, 2);

  // pattern Foo(...) does NOT match const-prefixed invocation:
  // OK:
  const Foo(1);
  // OK:
  const Foo(1, 2);

  // negative: different constructor name shouldn't match
  // OK:
  Bar(1);
}
