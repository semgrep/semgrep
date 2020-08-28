

class A {
  void foo(int a) {
    int y;

    bar(y);
    // no global in Java
    bar(x);
    bar(a);

  }

  void foo2() {
    // the addition of the local y in foo should not be visible here
    bar(y);
  }

}
