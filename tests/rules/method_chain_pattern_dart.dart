// Exercises the `. ...` chained-call ellipsis at rule level.

void demo(dynamic builder) {
  // ruleid: method-chain-pattern-dart
  builder.configure(opts).withTimeout(5).withRetry(3).build();

  // ok: method-chain-pattern-dart
  builder.configure(opts).withTimeout(5);

  // ok: method-chain-pattern-dart
  builder.setup(opts).withTimeout(5).build();
}
