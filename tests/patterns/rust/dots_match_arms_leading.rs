fn with_wildcard(x: i32) -> i32 {
    // ERROR:
    match x {
        0 => 1,
        1 => 2,
        _ => 3,
    }
}

fn no_wildcard(x: Option<i32>) -> i32 {
    match x {
        Some(v) => v,
        None => 0,
    }
}
