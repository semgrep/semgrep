char* test() {
  auto x;
  char *p = new char[source(x)];
  // ruleid: test
  sink(x);
  return p;
}
