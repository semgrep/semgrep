// ERROR:
void foo() {
  bar();
}

// ERROR:
int baz(int x) {
  return x + 1;
}

class C {
  // ERROR:
  String greet(String name) {
    return "hello";
  }
}
