//ERROR:
class A {
  foo() {
    return 'foo';
  }
}

// ERROR:
class A extends B {
  foo() {
    return 'bar';
  }
}

// OK:
class C {
  foo() {
    return 'bar';
  }
}