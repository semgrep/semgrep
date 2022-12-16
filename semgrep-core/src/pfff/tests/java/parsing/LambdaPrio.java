class Foo {
  void main() {
    // This is not allowed by the grammar.
    // return a -> 1 + a -> 2;
    return a -> 1 + (a -> 2);

  }
}
