// Verifies dart_canonical_segments on system libraries (`dart:...`):
// the wrapper grammar maps `dart:async` to DottedName ["dart"; "async"];
// Naming_AST reduces that to ["async"] so the pattern `async.Future(...)`
// matches under any alias.

import 'dart:async' as a;
import 'dart:async' as fut;

void main() {
  // ERROR:
  a.Future.delayed(const Duration(seconds: 1));

  // ERROR:
  fut.Future.delayed(const Duration(seconds: 1));

  // OK: different method on the same library
  //OK:
  a.Stream.fromIterable([]);
}
