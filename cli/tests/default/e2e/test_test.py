#
# Copyright (c) 2021-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
# Tests for 'semgrep --test' (accessible also now via 'osemgrep test').
# See https://semgrep.dev/docs/writing-rules/testing-rules/ for more info.
#
# See also test_fixtest.py for the autofix "fixtest" tests.
#
# TODO:
#  - test to detect wrong or missing ruleid: in a target/test file
#    (e.g., missed an annotation), with passed=false in the JSON
#  - test to detect invalid ruleid: annotation (wrong ruleid syntax)
#    like ruleid: without anything after, or a wrong character in rule id
#  - test do detect correctly annotations in different languages, using
#    different style of comments
#  - test to take a single directory and iterate over. This is actually
#    the main use case for --test and what we use in semgrep-rules/. However,
#    is a bit harder to test here given how run_semgrep_in_tmp() was
#    designed with always a 'config' and a 'target_name' parameter
#    (and the fact that the e2e rules and targets are in different dirs)
#    (maybe we could write this test in Testo instead and for osemgrep-only
#    once we removed test.py)
import json

import pytest
from tests.conftest import skip_on_windows
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


# It should report the passed checks in the JSON output
@pytest.mark.kinda_slow
def test_cli_test_basic(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/basic.yaml",
        options=["--test"],
        target_name="test_test/basic.py",
        output_format=OutputFormat.JSON,
    )

    posix_snapshot.assert_match(
        results,
        "results.json",
    )


# It should accept a directory as a target and rules in a different dir.
# TODO: this actually does not really test the ability to iterate over
# a directory (ability heavily used in semgrep-rules/scripts/run-test.sh)
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_cli_test_directory(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/test_test/directory/",
        options=["--test"],
        target_name="test_test/directory/",
        output_format=OutputFormat.JSON,
    )

    posix_snapshot.assert_match(
        results,
        "results.json",
    )


# It should output an "error" field with the right error message (timeout)
# in the JSON output.
# TODO: adding "--timeout", "1", does not seem to speedup things
@pytest.mark.slow
@pytest.mark.osemfail
@skip_on_windows  # better backslash replacement logic
def test_timeout(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/test_test/rule_that_timeout.yaml",
        options=["--test"],
        target_name="test_test/long.py",
        output_format=OutputFormat.JSON,
        assert_exit_code=1,
    )
    posix_snapshot.assert_match(
        results,
        "results.json",
    )


# It should look for a .test.yaml for test for rules about yaml.
# Indeed we can't have both a foo.yaml for the rule and foo.yaml for the test
# file in the same directory ... like we do for other languages like foo.yaml
# and foo.c (even though in our test infra the rule and test are in different
# directories so in theory we could).
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_cli_test_yaml_language(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/test_test/yaml_language/",
        options=["--test"],
        target_name="test_test/yaml_language/",
        output_format=OutputFormat.JSON,
    )
    posix_snapshot.assert_match(
        results,
        "results.json",
    )


# It should support rule filenames using multiple extensions/suffixes
# (e.g., this.that.rule.yaml)
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_cli_test_suffixes(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/test_test/suffixes/",
        options=["--test"],
        target_name="test_test/suffixes/",
        output_format=OutputFormat.JSON,
    )
    posix_snapshot.assert_match(
        results,
        "results.json",
    )


# It should support multiple annotations per line in a test file as
# in '#ruleid: rule1, rule2'
@pytest.mark.kinda_slow
def test_cli_test_multiple_annotations(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/test_test/overlapping_rules.yaml",
        options=["--test"],
        target_name="test_test/multiple_annotations.py",
        output_format=OutputFormat.TEXT,
        force_color=True,
    )
    posix_snapshot.assert_match(
        results,
        "results.txt",
    )


# It should ignore the 'paths:' 'include:' directive in the rule so it
# can be applied on a test file with a filename not satisfying the 'include:'.
@pytest.mark.kinda_slow
def test_cli_test_ignore_rule_paths(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/test_test/rule_with_paths_include_bar_xml.yaml",
        options=["--test"],
        target_name="test_test/foo.xml",
        output_format=OutputFormat.JSON,
    )
    posix_snapshot.assert_match(
        results,
        "results.json",
    )


# It should not report matches annotated with todook: in the JSON output.
# TODO? not sure why we do that actually.
# TODO: actually we're masking the <matches> in results.json, because
# of the use of realpath for the filenames, so this test is incomplete.
@pytest.mark.kinda_slow
def test_cli_todook_filtering(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/basic.yaml",
        options=["--test"],
        target_name="test_test/todook.py",
        output_format=OutputFormat.JSON,
    )

    posix_snapshot.assert_match(
        results,
        "results.json",
    )


_TEST_RULE = """\
rules:
- id: my-test-rule
  pattern: foo($X)
  message: found foo
  languages: [python]
  severity: INFO
"""


def _write_rule_and_target(tmp_path, target_text):
    rule = tmp_path / "myrule.yaml"
    rule.write_text(_TEST_RULE)
    target = tmp_path / "myrule.py"
    target.write_text(target_text)
    return rule, target


# A missed annotation should yield passed=false in the JSON report and a
# nonzero exit code (first TODO at the top of this file).
@pytest.mark.slow
def test_cli_test_failing_annotation_json(run_semgrep_in_tmp: RunSemgrep, tmp_path):
    _, target = _write_rule_and_target(tmp_path, "# ruleid: my-test-rule\nbar(1)\n")
    rule = tmp_path / "myrule.yaml"
    result = run_semgrep_in_tmp(
        config=str(rule),
        subcommand="scan",
        target_name=str(target),
        assume_targets_dir=False,
        options=["--test"],
        output_format=OutputFormat.JSON,
        assert_exit_code=1,
    )
    data = json.loads(result.stdout)
    # stdout paths are masked to <MASKED> by the runner; assert structurally
    assert len(data["results"]) == 1
    checks = next(iter(data["results"].values()))["checks"]
    assert checks["my-test-rule"]["passed"] is False


# An annotation naming a rule that does not exist in the config should fail
# with a rule id mismatch error.
@pytest.mark.slow
def test_cli_test_ruleid_mismatch(run_semgrep_in_tmp: RunSemgrep, tmp_path):
    _, target = _write_rule_and_target(tmp_path, "# ruleid: no-such-rule\nfoo(1)\n")
    rule = tmp_path / "myrule.yaml"
    _, stderr = run_semgrep_in_tmp(
        config=str(rule),
        subcommand="scan",
        target_name=str(target),
        assume_targets_dir=False,
        options=["--test"],
        output_format=OutputFormat.JSON,
        assert_exit_code=2,
    )
    assert "rule id mismatch" in stderr


# Configs without any paired test file should be listed in
# config_missing_tests without failing the run.
@pytest.mark.slow
def test_cli_test_reports_config_missing_tests(
    run_semgrep_in_tmp: RunSemgrep, tmp_path
):
    rules_dir = tmp_path / "myrules"
    rules_dir.mkdir()
    (rules_dir / "with_tests.yaml").write_text(_TEST_RULE)
    (rules_dir / "without_tests.yaml").write_text(
        _TEST_RULE.replace("my-test-rule", "other-rule")
    )
    targets_dir = tmp_path / "mytargets"
    targets_dir.mkdir()
    (targets_dir / "with_tests.py").write_text("# ruleid: my-test-rule\nfoo(1)\n")
    result = run_semgrep_in_tmp(
        config=str(rules_dir),
        subcommand="scan",
        target_name=str(targets_dir),
        assume_targets_dir=False,
        options=["--test"],
        output_format=OutputFormat.JSON,
        assert_exit_code=0,
    )
    data = json.loads(result.stdout)
    assert len(data["config_missing_tests"]) == 1
    assert len(data["results"]) == 1
    checks = next(iter(data["results"].values()))["checks"]
    assert checks["my-test-rule"]["passed"] is True
