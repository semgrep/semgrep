// A metavariable bound by `$COMMAND = ...` must unify with later uses of
// `$COMMAND` even when the binder comes from a Rust `let` (gh-8361).

fn plain_let(user_input: String) {
    let mut cmd = std::process::Command::new("sh");
    cmd.arg("-c");
    // ruleid: rust-let-metavariable-unification
    cmd.arg(user_input);
}

fn let_without_mut(user_input: String) {
    let cmd = std::process::Command::new("sh");
    cmd.arg("-c");
    // ruleid: rust-let-metavariable-unification
    cmd.arg(user_input);
}

fn let_with_type_annotation(user_input: String) {
    let mut cmd: std::process::Command = std::process::Command::new("sh");
    cmd.arg("-c");
    // ruleid: rust-let-metavariable-unification
    cmd.arg(user_input);
}

fn declaration_then_assignment(user_input: String) {
    let mut cmd;
    cmd = std::process::Command::new("sh");
    cmd.arg("-c");
    // ruleid: rust-let-metavariable-unification
    cmd.arg(user_input);
}

fn different_binder(user_input: String) {
    let mut cmd = std::process::Command::new("sh");
    let mut other = std::process::Command::new("bash");
    cmd.arg("-c");
    // ok: rust-let-metavariable-unification
    other.arg(user_input);
}
