from subprocess import CalledProcessError

import pytest

from semgrep.constants import OutputFormat


@pytest.mark.parametrize(
    "rule,target",
    [
        (
            "rules/patterns-from/taint-test.yaml",
            "patterns-from/taint-test.py",
        ),
        (
            "rules/patterns-from/pattern-addr-test.yaml",
            "patterns-from/pattern-addr-test.js",
        ),
        (
            "rules/patterns-from/message-override-test.yaml",
            "patterns-from/message-override-test.kt",
        ),
    ],
)
def test_patterns_from(run_semgrep_in_tmp, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target)[0],
        "results.json",
    )


def test_invalid_remote_rule(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/patterns-from/pattern-from-invalid.yaml")[0]
    assert excinfo.value.returncode == 2
    snapshot.assert_match(excinfo.value.stdout, "error.json")

    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(
            "rules/patterns-from/pattern-from-invalid.yaml",
            output_format=OutputFormat.TEXT,
        )[0]
    assert excinfo.value.returncode == 2
    snapshot.assert_match(excinfo.value.stderr, "error.txt")


def test_missing_remote_rule(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/patterns-from/pattern-from-missing.yaml")[0]
    assert excinfo.value.returncode == 2
    snapshot.assert_match(excinfo.value.stdout, "error.json")

    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(
            "rules/patterns-from/pattern-from-missing.yaml",
            output_format=OutputFormat.TEXT,
        )[0]
    assert excinfo.value.returncode == 2
    snapshot.assert_match(excinfo.value.stderr, "error.txt")
