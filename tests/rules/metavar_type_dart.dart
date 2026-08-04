void f(String s, int n) {
  // ruleid: metavar-type-dart
  print(s);
  // ok: metavar-type-dart
  print(n);
}

void g() {
  String label = "x";
  int counter = 0;
  // ruleid: metavar-type-dart
  print(label);
  // ok: metavar-type-dart
  print(counter);
}
