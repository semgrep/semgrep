
fn main() {
    // ERROR: match
    let result = Logic::Adder::new()
        .args(20)
        .args(30)
        .compute();
}
