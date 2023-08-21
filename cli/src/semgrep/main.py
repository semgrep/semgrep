from semgrep.cli import cli

# This is the entrypoint for the `pysemgrep` command.
def main() -> int:
    # To match the program usage help between pysemgrep (legacy)
    # and osemgrep (new) – and to hide complexity for our users -
    # here we specify `semgrep` as the program name for pysemgrep.
    cli(prog_name="semgrep")
    return 0
