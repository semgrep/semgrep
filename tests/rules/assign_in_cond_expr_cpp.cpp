void assignment_expression_parens() {
  void *data = get_taint();
  if (nondet) {
    // ruleid: assign-in-cond-expr
    sink(data);
  }

  void *data2 = NULL;
  // 11/10/2023: tree-sitter-cpp can't parse assignments in conditional
  // expression. only menhir cpp parser can parse this for now.
  while ((data2 = get_taint())) {
    // ruleid: assign-in-cond-expr
    sink(data2);
  }
}

class Foo {
  // added to check if menhir parser can parse virtual specifiers.
  // minimized version of https://semgrep.dev/playground/s/zzBG
  //
  // TODO: safe to remove once tree-sitter-cpp successfully parses this file.
  void bar() override {};
};

