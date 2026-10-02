// Verifies that the 3-statement polyglot pattern matches even when the
// gap between `$V = source();` and `sink($V);` contains many
// intermediate statements (5+ lines of unrelated work).

dynamic data;

void test() {
  // ERROR:
  data = source();
  log("started");
  log("more setup");
  log("doing things");
  log("almost there");
  log("finalizing");
  sink(data);
}
