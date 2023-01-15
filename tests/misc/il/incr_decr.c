void foo() {
  int i;
  for (i = 0; i < eval("10"); i++) {
    eval("i");
  }
}
