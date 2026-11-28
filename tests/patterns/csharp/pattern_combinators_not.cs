class C {
  void M(object o) {
    // ERROR:
    if (o is not null) { }
    if (o is not 0) { }
  }
}
