// Smoke test for grammar features added by the tree-sitter-dart bump:
// - Empty record literal `()` (Dart 3 record syntax)
// - Single-named-field record with trailing comma `(name: v,)`
// - Dot-shorthand `.foo` and `.new` (Dart 3.10)
// - Null-aware element `?key: value` in map literals
// - Null-assertion `!` inside a cascade chain
// - Unnamed library directive
// - Dotted and operator symbol literals (`#a.b.c`, `#==`)
// - `get`/`set` as identifiers (contextual keywords)

library;

class Empty {
  // empty record return type — used in Elm-style architecture.
  () makeEmpty() => ();

  // single-named-field record with trailing comma.
  ({int id}) makeNamed() => (id: 42,);

  // dot-shorthand inferred from context.
  Color get primary => .blue;
  Container build() => .new(child: .primary);

  // null-aware element in a map literal.
  Map<String, int> defaults(int? maybe) => {
        "always": 1,
        ?"sometimes": maybe,
      };

  // null-assertion `!` inside a cascade chain.
  void wire(Foo? foo) {
    foo!
      ..a = 1
      ..b!.c = 2;
  }

  // operator and dotted symbol literals.
  static const Symbol opEq = #==;
  static const Symbol nested = #a.b.c;

  // 'get'/'set' usable as identifiers in expression context.
  void useContextual() {
    final get = 1;
    final set = 2;
    print(get + set);
  }
}
