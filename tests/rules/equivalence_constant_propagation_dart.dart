class C {
  static const pwd = "password";
  static const other = "other";

  void f() {
    // ruleid: equivalence-constant-propagation-dart
    foo("password");
    // ruleid: equivalence-constant-propagation-dart
    foo(pwd);
    // ok: equivalence-constant-propagation-dart
    foo(other);
    // ok: equivalence-constant-propagation-dart
    foo("hello");
    // ok: equivalence-constant-propagation-dart
    bar("password");
  }
}
