void f(String name, String other, int count) {
  // ruleid: string-interp-metavar-dart
  print("hello $name");
  // ruleid: string-interp-metavar-dart
  print("hello $other");
  // ok: string-interp-metavar-dart
  print("hi $name");
  // ok: string-interp-metavar-dart
  print("hello");
  // ok: string-interp-metavar-dart
  print("count $count");
}
