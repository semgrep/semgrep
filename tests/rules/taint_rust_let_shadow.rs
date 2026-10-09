fn source() -> String { String::from("tainted") }
fn sink(x: &str) {}

fn test_with_shadow() {
    let x = source();
    let x = x;
    // ruleid: taint-rust-let-shadow
    sink(&x);
}

fn test_without_shadow() {
    let x = source();
    // ruleid: taint-rust-let-shadow
    sink(&x);
}

fn test_rename_not_shadow() {
    let x = source();
    let y = x;
    // ruleid: taint-rust-let-shadow
    sink(&y);
}

fn test_typed_shadow() {
    let x = source();
    let x: String = x;
    // ruleid: taint-rust-let-shadow
    sink(&x);
}
