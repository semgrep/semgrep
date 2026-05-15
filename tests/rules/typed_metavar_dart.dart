void f(String s, int n, dynamic d) {
  // ruleid: typed-metavar-dart
  print(s);
  // ok: typed-metavar-dart
  print(n);
  // ok: typed-metavar-dart
  print(d);
}

void g() {
  String name = "Alice";
  int count = 0;
  // ruleid: typed-metavar-dart
  print(name);
  // ok: typed-metavar-dart
  print(count);
}
