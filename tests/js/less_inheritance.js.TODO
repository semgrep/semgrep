//ERROR:
class A {
  constructor(name) {
    this.name = name;
  }

  foo() {
    return 'foo';
  }
}

//ERROR:
class B extends A {
  foo() {
    return 'bar';
  }
}

//ERROR:
class C extends B {
  foo() {
    return 'ccc';
  }
}