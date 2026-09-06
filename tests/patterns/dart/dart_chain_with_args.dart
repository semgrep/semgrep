// Verifies the `semgrep_dot_ellipsis_selector` rule for chained calls
// that carry argument lists. Pattern: `$O.start(...). ... .end(...)`
// should match any chain that begins with `.start(...)`, has any
// number of intermediate calls, and ends with `.end(...)`.

void test(dynamic builder, dynamic out) {
  // ERROR:
  out = builder.start(1).map((x) => x + 1).filter((x) => x > 0).end(2);

  //OK:
  out = builder.start(1).map((x) => x + 1).filter((x) => x > 0);

  //OK:
  out = builder.begin(1).map((x) => x + 1).end(2);
}
