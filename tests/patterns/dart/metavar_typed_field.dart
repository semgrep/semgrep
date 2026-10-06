class C {
  int count = 0;
  String label = "x";

  void report() {
    //ERROR: match
    log(count);

    //OK:
    log(label);
  }
}
