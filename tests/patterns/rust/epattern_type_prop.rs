fn foo(x : T) -> () {
  // ERROR: match
  sink(x);
}

fn foo() -> () {
  let x : T = ();
  // ERROR: match
  sink(x);
}
