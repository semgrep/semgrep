class C {
  void M(int o) {
    // ERROR:
    if (o is (1 or 2)) { }
    // Other operands: no match.
    if (o is (3 or 4)) { }
    if (o is (1 and 2)) { }
  }
}
