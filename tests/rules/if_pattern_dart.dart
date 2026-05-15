// The pattern `if (kDebugMode) { ... }` previously misparsed at the file
// level as a function definition (with `if` as the return type) and never
// matched real If nodes. Statement-keyword routing fix should make this
// match real `if (kDebugMode)` blocks.

class C {
  void m1() {
    // ruleid: if-pattern-dart
    if (kDebugMode) {
      print("dev mode");
    }
  }

  void m2() {
    // ruleid: if-pattern-dart
    if (kDebugMode) {
      log.fine("verbose");
      sendTelemetry();
    }
  }

  void m3() {
    // ok: if-pattern-dart
    if (count > 0) {
      print("positive");
    }
  }
}
