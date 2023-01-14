//ERROR:
class A {

  constructor(name) {
    this.name = name;
  }

  foo() {
    return 'foo';
  }
}

// OK:
class B {

  constructor(name) {
    this.name = name;
  }

  foo() {
    return 'foo';
  }
}