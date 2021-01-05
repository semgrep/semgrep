class Foo {

  void m(int a) {
    return a;
  }

  void bar(int a) {
    // implicit this.foo() added
    foo(a);
  }
}
