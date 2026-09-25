// Companion to dart_import_package_collision.dart: verifies the
// unsafe_canonicals guard is scoped per canonical prefix, not file-wide.
// This file has a genuine `http` collision (like the other test), but
// also imports `dart:async`, which has no collision here and must still
// collapse to its usual canonical prefix and match normally.

import 'package:http/http.dart' as h;
import 'package:other/http.dart' as o;
import 'dart:async' as a;

void main() {
  // OK: unrelated to the async.Future.delayed pattern under test.
  //OK:
  h.get('https://example.com');

  // OK: unrelated to the async.Future.delayed pattern under test.
  //OK:
  o.get('https://example.com');

  // ERROR:
  a.Future.delayed(const Duration(seconds: 1));
}
