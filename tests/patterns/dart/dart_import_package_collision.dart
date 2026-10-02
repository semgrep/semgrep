// Verifies the Naming_utils.unsafe_canonicals guard in Naming_AST: two
// DIFFERENT packages whose main files share a basename --
// `package:http/http.dart` and `package:other/http.dart` -- both naively
// reduce to the "http" prefix via dart_canonical_segments. Once both
// appear in the same file, neither collapses to "http" anymore, so a
// pattern written against the canonical prefix does not spuriously match
// the unrelated package. As an accepted trade-off, it also stops matching
// the real `http` package's calls in this file, since the import URI
// alone can't tell which "http" a rule author meant.

import 'package:http/http.dart' as h;
import 'package:other/http.dart' as o;

void main() {
  // OK: ambiguous with the import below; the canonical-prefix equivalence
  // backs off for both rather than guessing which "http" is real.
  //OK:
  h.get('https://example.com');

  // OK: a different package must never be treated as `http`.
  //OK:
  o.get('https://example.com');
}
