class C {
  void M(int[] a) {
    // ERROR:
    if (a is [1, 9]) { }
    // ERROR:
    if (a is [1, 5, 7, 9]) { }
    // Wrong last element: no match.
    if (a is [1, 5, 8]) { }
  }
}
