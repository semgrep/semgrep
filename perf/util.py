def get_semgrep_version() -> str:
    import subprocess

    return (
        subprocess.run(["semgrep", "--version"], capture_output=True)
        .stdout.decode("utf-8")
        .strip()
    )
