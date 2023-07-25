fn foo(x : T) -> () {
  sink(x);
}

fn foo() -> () {
  let x : T = ();
  sink(x);
}