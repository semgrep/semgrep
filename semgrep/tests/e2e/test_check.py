from pathlib import Path


def test_basic_rule__local(run_semgrep, snapshot):
    snapshot.assert_match(run_semgrep("rules/eqeq.yaml"), "results.json")


def test_basic_rule__relative(run_semgrep, snapshot):
    snapshot.assert_match(
        run_semgrep("rules/../rules/eqeq.yaml"), "results.json",
    )


def test_basic_rule__absolute(run_semgrep, snapshot):
    snapshot.assert_match(
        run_semgrep(Path.cwd() / "rules" / "eqeq.yaml"), "results.json",
    )


def test_terminal_output(run_semgrep, snapshot):
    snapshot.assert_match(run_semgrep("rules/eqeq.yaml", use_json=False), "output.txt")


def test_url_rule(run_semgrep, snapshot):
    snapshot.assert_match(
        run_semgrep(
            "https://raw.githubusercontent.com/returntocorp/semgrep-rules/develop/template.yaml",
        ),
        "results.json",
    )


def test_registry_rule(run_semgrep, snapshot):
    snapshot.assert_match(
        run_semgrep("r2c"), "results.json",
    )
