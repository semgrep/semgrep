class C {
  void M(int[] xs) {
    // ERROR:
    if (xs is [1, ..] rest) { }
    // Different name: no match.
    if (xs is [1, ..] other) { }
    // No designation: no match.
    if (xs is [1, ..]) { }
  }
}
