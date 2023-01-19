class Foo {

  void m(int a) {
    return a;
  }

  void bar(int a) {
    // we used to implicit convert this in this.foo()
    // but we don't anymore, as we were not doing it in tree-sitter
    foo(a);
  }
}
