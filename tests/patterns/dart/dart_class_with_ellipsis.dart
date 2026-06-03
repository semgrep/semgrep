// Verifies that the polyglot `class $X { ... }` pattern from
// `metavar_anno` (and similar) parses Dart code with various class
// member kinds — fields, methods, getters, constructors, all mixed.
// The `_class_member_definition` wrapper-grammar fix is what allows
// the `...` placeholder to stand in for any class member.

// ERROR:
class WithFieldAndMethod {
  int x = 0;
  void f() {}
}

// ERROR:
class WithCtorAndGetter {
  final int x;
  WithCtorAndGetter(this.x);
  int get doubled => x * 2;
}

// ERROR:
class WithMixedMembers {
  int a = 0;
  static int counter = 0;
  WithMixedMembers();
  int get current => a;
  set current(int v) { a = v; }
  void increment() { a++; }
}

// ERROR:
abstract class Empty {}
