class C {
  void M(object a) {
    switch (a) {
      // ERROR:
      case Dog(var x) d:
        Use(d);
        break;
      case Cat(var x) d:
        Use(d);
        break;
    }
  }
}
