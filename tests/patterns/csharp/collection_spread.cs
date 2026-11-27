class C {
  void M(int[] xs) {
    // ERROR:
    int[] a = [1, ..xs];
    // A plain element is not a spread: no match.
    int[] b = [1, xs];
  }
}
