void test(dynamic o, dynamic f) {
  //ERROR: match
  f = o.foo().m().h().bar().z();

  //ERROR: match
  f = o.foo().bar();

  // negative: this one does not contain the bar() segment
  //OK:
  f = o.foo().m().h().z();
}
