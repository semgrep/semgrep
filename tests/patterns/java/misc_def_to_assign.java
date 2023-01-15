class A {
  void f() {
    //ERROR: match
    Foo foo = bar();
    return foo;
  }
}
