class TestCases {

  // -------------------------
  // F
  // -------------------------
  void F(Object? x) {
    // Dart has no tuple patterns, so we simulate them using List checks.
    if (x is List && x.length == 2 && x[0] is int && x[1] is int) {
      int y = x[0] as int;
      int z = x[1] as int;

      // ruleid: taint
      sink(y);
    } else {
      sink(5);
    }
  }

  // -------------------------
  // G
  // -------------------------
  void G(Object? x) {
    // Pattern: ((int y, int z), int w)
    if (x is List &&
        x.length == 2 &&
        x[0] is List &&
        (x[0] as List).length == 2 &&
        (x[0] as List)[0] is int &&
        (x[0] as List)[1] is int &&
        x[1] is int) {
      int y = (x[0] as List)[0] as int;
      int z = (x[0] as List)[1] as int;
      int w = x[1] as int;

      // ruleid: taint
      sink(y);
    } else {
      // ruleid: taint
      sink(x);
    }
  }

  // -------------------------
  // H
  // -------------------------
  void H(Object? x) {
    // Pattern: (int y, int z) when sink(y)
    if (x is List &&
        x.length == 2 &&
        x[0] is int &&
        x[1] is int) {
      int y = x[0] as int;
      int z = x[1] as int;

      // ruleid: taint
      if (sink(y) != null) {
        // ruleid: taint
        sink(z);
      }
    } else {
      // default: break
    }
  }
}
