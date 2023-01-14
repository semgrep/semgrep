use std::process::Command;

fn main() -> io::Result<()> {
    // ERROR: match
    let proc = Command::new("semgrep-core")
        .args(["-l", "rust"])
        .args(["-rules", "test.yaml"])
        .output()
        .expect("failed to execute process");

    Ok(())
}
