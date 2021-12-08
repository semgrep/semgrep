from subprocess import CalledProcessError

import pytest


def test_regex_rule__nosemgrep(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-nosemgrep.yaml", target_name="basic/regex-nosemgrep.txt"
        )[0],
        "results.json",
    )


def test_nosem_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(run_semgrep_in_tmp("rules/nosem.yaml")[0], "results.json")


def test_nosem_rule_unicode(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/nosem-unicode.yaml", target_name="advanced_nosem/nosem-unicode.py"
        )[0],
        "results.json",
    )


def test_nosem_rule__invalid_id(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/nosem.yaml", target_name="nosem_invalid_id")
    assert excinfo.value.returncode == 2
    snapshot.assert_match(excinfo.value.stderr, "error.txt")
    snapshot.assert_match(excinfo.value.stdout, "error.json")


def test_nosem_rule__with_disable_nosem(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/nosem.yaml", options=["--disable-nosem"])[0],
        "results.json",
    )
