void main() {
  String x = "hello";
  int y = 1;
  String? maybe = "world";
  var inferred = "inferred";
  //ERROR: match
  foo(x);
  //ERROR: match
  foo(inferred);
  // Nullable `String?` is treated as a `String` for typed-metavar
  // matching purposes — confirms typed-metavar equivalence ignores
  // the trailing `?` (matches Dart's flow-sensitive promotion).
  //ERROR: match
  foo(maybe);

  //OK:
  foo(y);
}
