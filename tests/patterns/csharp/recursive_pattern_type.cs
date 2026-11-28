class T {
  void M(object a) {
    // ERROR:
    switch (a) {
      case Dog(var x) d:
        Use(d);
        break;
    }
    // A different type must not match.
    switch (a) {
      case Cat(var x) d:
        Use(d);
        break;
    }
  }
}
