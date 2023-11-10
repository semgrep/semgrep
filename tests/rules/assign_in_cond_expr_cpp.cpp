void assignment_expression_parens() {
  void *data = get_taint();
  if (nondet) {
    // ruleid: assign-in-cond-expr
    sink(data);
  }

  void *data2 = NULL;
  while ((data2 = get_taint())) {
    // ruleid: assign-in-cond-expr
    sink(data2);
  }
}

class Foo {
  void bar() override {};
};

