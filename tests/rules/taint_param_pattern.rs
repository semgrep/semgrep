
fn f ((a, (b, c)) | d : t1, y : t2, z : t3) -> () {
  // ruleid: taint-param-pattern
  sink(a);
  // ruleid: taint-param-pattern
  sink(b);
  // ruleid: taint-param-pattern
  sink(c);
  // ruleid: taint-param-pattern
  sink(d);
  // ruleid: taint-param-pattern
  sink(y);

  sink(z);
}