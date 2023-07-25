fn foo() -> () {

  let x = source;

  if condition == 1 {
      // ruleid: taint-rust-returns
      return x;
  }

  // ruleid: taint-rust-returns
  return x;
}
