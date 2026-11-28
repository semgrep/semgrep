class C {
  void M(bool a, bool b, bool c) {
    if (a) { }
#if X
    else if (b) { }
    else if (c) { }
#endif
    else {
      // ERROR:
      sink();
    }
  }
}
