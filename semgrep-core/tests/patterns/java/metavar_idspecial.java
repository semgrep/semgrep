class Bar {
  Bar() {
  }
}

class Foo extends Bar {
  Foo() {
    // MATCH:
    this(2);
  }

  Foo(int x) {
    // MATCH:
    super();
    // MATCH:
    this.normalMethod();
  }

  void normalMethod() {
  }
}
