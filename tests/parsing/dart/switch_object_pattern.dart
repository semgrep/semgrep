// Regression test for Dart 3 object patterns in switch expressions.
// The previously-pinned tree-sitter-dart grammar rejected empty-paren
// object patterns ('_MyClass()' arms), causing parse failures on real
// Flutter code. Upstream tree-sitter-dart PR #93 fixed this by changing
// `commaSep1TrailingComma` to `commaSepTrailingComma` on `object_pattern`.

sealed class Library {
  const Library();
}

class _MaterialLibrary extends Library {}
class _CupertinoLibrary extends Library {}
class _OtherLibrary extends Library {}

bool canImport(Library lib) {
  return switch (lib) {
    _MaterialLibrary() => true,
    _CupertinoLibrary() => true,
    _OtherLibrary() => false,
  };
}

class Box<T> {
  final T value;
  Box(this.value);
}

String describe(Object o) {
  return switch (o) {
    Box<int>(value: final v) when v > 0 => 'positive box',
    Box(value: final v) => 'box of $v',
    null => 'null',
    _ => 'other',
  };
}

void main() {
  print(canImport(_MaterialLibrary()));
  print(describe(Box<int>(42)));
}
