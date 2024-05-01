# Tests for 'semgrep --test' (accessible also now via 'osemgrep test').
# See https://semgrep.dev/docs/writing-rules/testing-rules/ for more info.
#
# See also test_fixtest.py for the autofix "fixtest" tests.
#
# TODO:
#  - test to detect wrong ruleid: in target file (missed an annotation)
#    with passed=false in the JSON
#  - test to detect invalid ruleid: (wrong ruleid syntax)
#    like ruleid: without anything after, wrong character in rule id, etc.
#  - test to take a single directory and iterate over (what we do
#    in semgrep-rules/), but harder given how run_semgrep_in_tmp() was
#    designed with always a 'config' and a 'target_name' parameter
#    (and the fact that the e2e rules and targets are in different dirs)
import os
import subprocess
from pathlib import Path

import pytest
from tests.fixtures import RunSemgrep
from tests.semgrep_runner import SEMGREP_BASE_SCAN_COMMAND

from semgrep.constants import OutputFormat


# test the basic JSON output of --test reporting the passed checks
@pytest.mark.kinda_slow
def test_cli_test_basic(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/basic.yaml",
        options=["--test"],
        target_name="cli_test/basic.py",
        output_format=OutputFormat.JSON,
    )

    snapshot.assert_match(
        results,
        "results.json",
    )


# The --test flag can accept a directory as a target
# TODO: this actually does not really test the ability to iterate over
# a directory (ability heavily used in semgrep-rules/scripts/run-test.sh)
@pytest.mark.kinda_slow
def test_cli_test_directory(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/directory/",
        options=["--test"],
        target_name="cli_test/directory/",
        output_format=OutputFormat.JSON,
    )

    snapshot.assert_match(
        results,
        "results.json",
    )


# Test that the JSON output will contains an "error" field with the right
# error message (timeout).
# TODO: adding "--timeout", "1", does not seem to speedup things
@pytest.mark.slow
@pytest.mark.osemfail
def test_timeout(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/rule_that_timeout.yaml",
        options=["--test"],
        target_name="cli_test/long.py",
        output_format=OutputFormat.JSON,
        assert_exit_code=1,
    )
    snapshot.assert_match(
        results,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_cli_test_yaml_language(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/language/",
        options=["--test"],
        target_name="cli_test/language/",
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(
        results,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_cli_test_suffixes(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/suffixes/",
        options=["--test"],
        target_name="cli_test/suffixes/",
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(
        results,
        "results.json",
    )


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_cli_test_multiple_annotations(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/overlapping_rules.yaml",
        options=["--test"],
        target_name="cli_test/multiple_annotations.py",
        output_format=OutputFormat.TEXT,
        force_color=True,
    )
    snapshot.assert_match(
        results,
        "results.txt",
    )


@pytest.mark.slow
def test_cli_test_from_entrypoint(snapshot):
    env = {}
    env["PATH"] = os.environ.get("PATH", "")

    cmd = SEMGREP_BASE_SCAN_COMMAND + [
        "--test",
        "--config",
        "rules/basic.yaml",
        "targets/cli_test/basic.py",
    ]
    result = subprocess.run(
        cmd,
        cwd=Path(__file__).parent,
        capture_output=True,
        encoding="utf-8",
        check=True,
        env=env,
        timeout=15,
    )
    snapshot.assert_match(result.stdout, "output.txt")


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_cli_test_match_rules_same_message(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/match_rules_same_message/rules.yml",
        target_name="cli_test/basic.py",
        output_format=OutputFormat.TEXT,
        force_color=True,
    )
    snapshot.assert_match(
        results,
        "results.txt",
    )


@pytest.mark.kinda_slow
def test_cli_test_ignore_rule_paths(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/ignore_rule_paths/",
        options=["--test"],
        target_name="cli_test/ignore_rule_paths/",
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(
        results,
        "results.json",
    )
