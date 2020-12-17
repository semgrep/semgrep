//ERROR: match
class A {
  private FooType foo;
  void foo1() {
    this.foo.query();
  }
  void foo2() {
    foo.query();
  }
}
