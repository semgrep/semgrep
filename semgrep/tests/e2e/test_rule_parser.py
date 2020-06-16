from subprocess import CalledProcessError

import pytest
from tests.conftest import TESTS_PATH

syntax_dir = TESTS_PATH / "e2e" / "rules" / "syntax"
syntax_passes = [f.with_suffix("").name for f in syntax_dir.glob("good*.yaml")]
syntax_fails = [
    f.with_suffix("").name for f in syntax_dir.glob("*.yaml") if "good" not in f.name
]


@pytest.mark.parametrize("filename", syntax_passes)
def test_rule_parser__success(run_semgrep_in_tmp, snapshot, filename):
    run_semgrep_in_tmp(f"rules/syntax/{filename}.yaml")


@pytest.mark.parametrize("filename", syntax_fails)
def test_rule_parser__failure(run_semgrep_in_tmp, snapshot, filename):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(f"rules/syntax/{filename}.yaml")
    assert excinfo.value.returncode != 0
    snapshot.assert_match(str(excinfo.value.returncode), "returncode.txt")


@pytest.mark.parametrize("filename", syntax_fails)
def test_rule_parser__failure__error_messages(run_semgrep_in_tmp, snapshot, filename):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(f"rules/syntax/{filename}.yaml", stderr=True)

    with pytest.raises(CalledProcessError) as excinfo_in_color:
        run_semgrep_in_tmp(
            f"rules/syntax/{filename}.yaml", options=["--force-color"], stderr=True
        )
    snapshot.assert_match(excinfo.value.output, "error.txt")

    if excinfo_in_color.value.output != excinfo.value.output:
        snapshot.assert_match(excinfo_in_color.value.output, "error-in-color.txt")
