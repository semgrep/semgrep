// Underscores as visual separators in numeric literals (Rust RFC 3101).
// Exercises Parsed_int for radix-prefix underscores (e.g. 0x_dead_beef) and
// between-digit separators.

fn decimal() {
    let _ = 1_000_000;
    let _ = 98_222;
}

fn hex_oct_bin() {
    let _ = 0x_dead_beef;
    let _ = 0xDEAD_BEEF;
    let _ = 0x_1;
    let _ = 0o_755;
    let _ = 0b_1010_1010;
}

// Rust allows multiple underscores after the radix prefix (unlike Python).
fn multiple_underscores_after_prefix() {
    let _ = 0b________1;
}
