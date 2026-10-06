// Verifies dart_canonical_segments on a multi-segment package URI:
// `package:foo/src/utils/helpers.dart` decomposes to
//   ["package"; "foo"; "src"; "utils"; "helpers.dart"]
// and reduces to ["helpers"] — the basename without `.dart`. The
// pattern `helpers.parse(...)` matches the aliased call.

import 'package:foo/src/utils/helpers.dart' as utils;

void main() {
  // ERROR:
  utils.parse('input');

  // OK: same import, different method
  //OK:
  utils.serialize('input');
}
