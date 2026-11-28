class C {
  void M(object o) {
    // ERROR:
    var a = o is Foo;
    // ERROR:
    if (o is Foo) { }
    // Another type: no match.
    if (o is Bar) { }
    // A constant pattern: no match.
    if (o is null) { }
  }
}
