class C {
  void M(object o) {
    // ERROR:
    if (o is Point(X: 1, 2)) { }
    // A different name, or none: no match.
    if (o is Point(Y: 1, 2)) { }
    if (o is Point(1, 2)) { }
  }
}
