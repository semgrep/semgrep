fn test() -> () {
  let x = source;

  let a1 = foo!(&x);
  let a2 = foo!(&x, 0);
  let a3 = foo!(0, &x);
  let a_bad = foo!(&x,);

  let b1 = foo!(*x);
  let b2 = foo!(*x, 0);
  let b3 = foo!(0, *x);
  let b_bad = foo!(*x,);

  // ruleid: rust-macro-token-args
  sink(a1);
  // ruleid: rust-macro-token-args
  sink(a2);
  // ruleid: rust-macro-token-args
  sink(a3);
  // ruleid: rust-macro-token-args
  sink(b1);
  // ruleid: rust-macro-token-args
  sink(b2);
  // ruleid: rust-macro-token-args
  sink(b3);

  // ok: rust-macro-token-args
  sink(a_bad);
  // ok: rust-macro-token-args
  sink(b_bad);
}
