// End-to-end taint test: rule pattern uses the canonical library prefix
// `http`, but the code imports `package:http/http.dart` under various
// aliases. The Dart equivalence_naming_import wiring is what makes the
// rule pattern unify with the aliased call sites.

import 'package:http/http.dart' as h;
import 'package:http/http.dart' as client;

void demo() async {
  // `h.get(...)` flows into eval.
  var resp = await h.get('https://example.com');
  // ruleid: import-alias-taint-dart
  eval(resp);

  // Second alias for the same library — canonical is still `http`.
  var other = await client.get('https://example.com');
  // ruleid: import-alias-taint-dart
  eval(other);

  // post() is not a source.
  var post = await h.post('https://example.com');
  // ok: import-alias-taint-dart
  eval(post);
}
