class C {
  void M(int o) {
    // ERROR:
    if (o is > 0 and < 10) { }
    // ERROR:
    if (o is (> 0 and < 10)) { }
    if (o is > 0 and < 20) { }
    if (o is >= 0 and < 10) { }
  }
}
