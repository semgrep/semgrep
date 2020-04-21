//ERROR:
class A {
  foo = "bar";

  constructor(name) {
    this.name = name;
  }

  foo() {
    return 'foo';
  }
}

// OK:
class B {
  foo = "bar";

  constructor(name) {
    this.name = name;
  }

  foo() {
    return 'foo';
  }
}