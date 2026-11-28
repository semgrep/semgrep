class C {
  void M(int[] xs) {
    // ERROR:
    int[] a = [1, 2, 3];
    // ERROR:
    int[] b = [1, ..xs, 9];
    // ERROR:
    int[] c = [1];
    // Does not start with 1: no match.
    int[] d = [2, 1];
  }
}
