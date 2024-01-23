void ok() {
  auto x = tainted();
  clean(x->parameter);
  // ok: test
  sink(x->parameter);
}

void bad() {
  auto x = tainted();
  // ruleid: test
  sink(x->parameter);
}
