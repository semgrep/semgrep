class C {
  // ERROR:
  void A(ref int x) { }
  // No `ref`: no match.
  void B(int x) { }
  void D(out int x) { x = 0; }
}

static class E {
  // ERROR:
  static void Ext(this ref int x) { }
}
