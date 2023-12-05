fn test() -> () {
  let x = source;

  let a1 = foo!(&x);
  let a2 = foo!(&x, 0);
  let a3 = foo!(0, &x);
  let a_bad = foo!(&x,);
  let a_bad2 = foo!(*&x);

  let b1 = foo!(*x);
  let b2 = foo!(*x, 0);
  let b3 = foo!(0, *x);
  let b_bad = foo!(*x,);

  let c1 = foo!(x.y);
  let c2 = foo!(0, x.y);
  let c3 = foo!(x.y, 0);
  let c_bad = foo!(x..y);

  let all = foo!(&x.y.z);

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

  // ruleid: rust-macro-token-args
  sink(c1);
  // ruleid: rust-macro-token-args
  sink(c2);
  // ruleid: rust-macro-token-args
  sink(c3);

  // ruleid: rust-macro-token-args
  sink(all);

  // ok: rust-macro-token-args
  sink(a_bad);
  // ruleid: rust-macro-token-args
  sink(a_bad2);
  // ok: rust-macro-token-args
  sink(b_bad);
  // ok: rust-macro-token-args
  sink(c_bad);
}
