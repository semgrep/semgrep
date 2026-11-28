class C {
  void M(int o) {
    // ERROR:
    if (o is (> 0) m) { }
    // Different name: no match.
    if (o is (> 0) k) { }
  }
}
