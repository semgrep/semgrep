fn tail_expr(x: i32) -> i32 {
    // ERROR:
    match x {
        0 => 1,
        _ => 2,
    }
}

fn stmt_pos(x: i32) {
    // ERROR:
    match x {
        0 => println!("zero"),
        _ => println!("other"),
    }
}

fn let_bound(x: i32) -> i32 {
    // ERROR:
    let y = match x {
        0 => 1,
        _ => 2,
    };
    y
}
