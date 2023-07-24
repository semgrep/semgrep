fn f0() {
}

// TODO: This should probably be considered a bug for now. I think we
// could fix this by wrapping an option around the argument list in
// generic AST.
// MATCH: 
#[no_call]
fn f1() {
}

// MATCH:
#[no_arg()]
fn f2() {
}

// MATCH:
#[with_arg("foo")]
fn f3() {
}

// MATCH:
#[with_args("foo", "bar")]
fn f4() {
}


