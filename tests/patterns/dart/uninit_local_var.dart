// Local declarations without an initializer must survive into the AST so
// patterns like `String $X;` can match them. Regression test for the bug
// where `map_initialized_variable_definition_unwrapped` returned [] when
// the optional `= expr` was absent.

class C {
  void run() {
    // ERROR:
    String first;
    // ERROR:
    String second;
    first = "hi";
    second = "there";
  }

  void otherTypes() {
    int n;
    bool b;
    print(n);
    print(b);
  }
}
