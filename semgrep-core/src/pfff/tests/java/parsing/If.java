class foo {
  void foo() {
    x = 1;

    if (1)
      x = 2;

    else
      x = 3;
      // x; NOT ALLOWED
  }
}
