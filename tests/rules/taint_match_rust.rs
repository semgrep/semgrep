fn source() -> String {
    String::from("tainted")
}

fn sink(x: String) {}

// Basic: taint flows through match with constructor destructuring
fn test_match_constructor() {
    let tainted = source();
    let wrapped = Some(tainted);

    match wrapped {
        // ruleid: taint-match-rust
        Some(x) => sink(x),
        None => (),
    }
}

// Taint flows through Ok/Err destructuring
fn test_match_result() {
    let tainted: Result<String, String> = Ok(source());

    match tainted {
        // ruleid: taint-match-rust
        Ok(value) => sink(value),
        Err(_) => (),
    }
}

// Taint through Err variant
fn test_match_result_err() {
    let tainted: Result<String, String> = Err(source());

    match tainted {
        Ok(_) => (),
        // ruleid: taint-match-rust
        Err(e) => sink(e),
    }
}

// Direct scrutinee (not via let binding)
fn test_match_direct() {
    match Some(source()) {
        // ruleid: taint-match-rust
        Some(x) => sink(x),
        None => (),
    }
}

// Wildcard pattern should not propagate (no variable to bind)
fn test_match_wildcard() {
    let clean = String::from("clean");
    let wrapped = Some(clean);

    match wrapped {
        // ok: taint-match-rust
        Some(_) => sink(String::from("not tainted")),
        None => (),
    }
}

// Variable binding without constructor
fn test_match_variable() {
    let tainted = source();

    match tainted {
        // ruleid: taint-match-rust
        x => sink(x),
    }
}

// Match used as expression
fn test_match_expression() {
    let tainted = source();
    let wrapped = Some(tainted);

    let result = match wrapped {
        Some(x) => x,
        None => String::from("default"),
    };

    // ruleid: taint-match-rust
    sink(result);
}

// Nested match - taint should flow through
fn test_nested_match() {
    let tainted = source();
    let wrapped = Some(Ok(tainted));

    match wrapped {
        Some(inner) => {
            match inner {
                // ruleid: taint-match-rust
                Ok(value) => sink(value),
                Err(_) => (),
            }
        }
        None => (),
    }
}

// Clean data should not trigger
fn test_clean_data() {
    let clean = String::from("clean");
    let wrapped = Some(clean);

    match wrapped {
        // ok: taint-match-rust
        Some(x) => sink(x),
        None => (),
    }
}
