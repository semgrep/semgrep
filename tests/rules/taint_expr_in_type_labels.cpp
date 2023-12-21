char* ok() {
  auto ev = tainted();
  char *p = new char[strlen(ev->parameter)];
  // ok: test
  sink(ev->parameter);
  return p;
}

void bad() {
  auto ev = tainted();
  // ruleid: test
  sink(ev->parameter);
}
