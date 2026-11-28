class C {
  void M(Color c, int n) {
    // ERROR:
    switch (c) {
      case Color.Red:
        break;
    }
    // ERROR:
    switch (n) {
      case -1:
        break;
    }
    // ERROR:
    switch (n) {
      case 1 + 2:
        break;
    }
  }
}
