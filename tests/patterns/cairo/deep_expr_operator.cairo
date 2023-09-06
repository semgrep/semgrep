
fn foo() {
  //ERROR:
  let x = foo(bar(1 + 42));

  let y = foo(1);
}
