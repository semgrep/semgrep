// Verifies the multi-statement pattern path picks up the metavariable
// across statements of different kinds — the ellipsis between
// `$V = open();` and `close($V);` must transparently skip over an `if`
// statement and a `for` statement that don't reference $V.

dynamic resource;
List<int> items = [];

void test() {
  // ERROR:
  resource = open();

  if (items.isEmpty) {
    log("warmup");
  }

  for (var i = 0; i < items.length; i++) {
    process(items[i]);
  }

  close(resource);
}
