class Obj {
  void method<T>(T arg) {}
}

void main() {
  var obj = Obj();

  // Before the tree-sitter-dart pin bump, `obj.method<int>(42)` mis-parsed
  // as the relational chain `(obj.method < int) > (42)` instead of a call,
  // so it never matched a call-shaped pattern at all. The pin bump fixes
  // the parse; map_selector_choice's add_type_arguments_to_callee then
  // threads `<int>` onto the callee's name (name_last), the same way
  // Rust/Move-on-Sui/Move-on-Aptos attach turbofish-style type arguments,
  // so a pattern can actually distinguish it from a different type arg.
  //ERROR: match
  obj.method<int>(42);

  // OK: different type argument -- must not be conflated with <int>.
  //OK:
  obj.method<String>('x');

  //OK: not a call to `method`
  obj.other<int>(42);
}
