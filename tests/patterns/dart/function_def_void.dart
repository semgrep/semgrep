// Top-level function with the matching name and shape.
// ERROR:
void test() {
  foo();
}

void run() {
  bar();
}

class A {
  // ERROR:
  void test() {
    foo();
  }

  void other() {
    bar();
  }
}
