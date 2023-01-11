
void test_dyncall1() {
  f(1);
  (*f)(1);
  x->f(1);
  x[1](1);
}

void test_dyncall_local(fn f) {
  f(1);
}
