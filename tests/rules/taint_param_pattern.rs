
fn f ((a, Foo {b, c: c2}) | d : t1, y : t2, z : t3) -> () {
  // todos caused by https://github.com/returntocorp/semgrep/pull/8442
  // todo: taint-object-destructure
  sink(a);
  // todo: taint-param-pattern
  sink(b);
  // ok: taint-param-pattern
  sink(c);
  // todo: taint-param-pattern
  sink(c2);
  // todo: taint-param-pattern
  sink(d);
  // todo: taint-param-pattern
    // todo: taint-object-destructure
  sink(y);

  sink(z);
}