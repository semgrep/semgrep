class C {
  void M() {
    // ERROR:
    a();
#if X
    b();
#endif
  }
  void N() {
    // ERROR:
    a();
#if X
    b();
#else
    c();
#endif
  }
}
