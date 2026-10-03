fn f() -> (i32, i32) {
    (1, 2)
}

struct Point {
    x: i32,
    y: i32,
}

fn main() {
    // ERROR: match
    let z = 1;
    // ERROR: match
    let p = Point { x: 1, y: 2 };
    // ERROR: match
    let (a, b) = f();
    // ERROR: match
    let Point { x, y } = p;
    // ERROR: match
    let _ = (z, a, b, x, y);
}
