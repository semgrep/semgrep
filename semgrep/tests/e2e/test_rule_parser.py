from subprocess import CalledProcessError

import pytest


@pytest.mark.parametrize("filename", ["good", "good_info_severity", "good_metadata"])
def test_rule_parser__success(run_semgrep_in_tmp, snapshot, filename):
    run_semgrep_in_tmp(f"rules/syntax/{filename}.yaml")


@pytest.mark.parametrize(
    "filename",
    ["bad1", "bad2", "bad3", "bad4", "badpattern", "badpaths1", "badpaths2"],
)
def test_rule_parser__failure(run_semgrep_in_tmp, snapshot, filename):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(f"rules/syntax/{filename}.yaml")
    assert excinfo.value.returncode != 0


@pytest.mark.parametrize(
    "filename",
    [
        pytest.param(
            "bad1",
            marks=pytest.mark.xfail(
                reason="https://github.com/returntocorp/semgrep/issues/678"
            ),
        ),
        pytest.param(
            "bad2", marks=pytest.mark.xfail(reason="prints set with arbitrary order")
        ),
        "bad3",
        "bad4",
        "badpattern",
        "badpaths1",
        "badpaths2",
    ],
)
def test_rule_parser__failure__error_messages(run_semgrep_in_tmp, snapshot, filename):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(f"rules/syntax/{filename}.yaml", stderr=True)
    snapshot.assert_match(excinfo.value.output, "error.txt")
