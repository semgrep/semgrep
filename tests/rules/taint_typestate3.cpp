void test() {
  while (nondet) {
    int *v1 = new int;
    int *v2;
    v2 = new int;
    // ok: test
    delete v1;
    // ok: test
    delete v2;
  }
}
