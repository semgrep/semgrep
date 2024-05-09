# Testing the fixtest feature.
# See https://semgrep.dev/docs/writing-rules/testing-rules/#testing-autofix
#
# TODO:
#  - rename those test_fixtest_testx_xx_json; give meaningful names so we
#    know what they test and adjust also accordingly the targets/testx.py
#    (but do not use targets/test_whatever.py because then those targets are
#     considered pytest candidates)
#  - add test to detect missing_config_fixtest when rule use fix-regex
import re

import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


# It should report passing fixtests in the text output.
# TODO: rename test_passed_text_output
@pytest.mark.kinda_slow
def test_fixtest_test1_no_json(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/fixtest/basic_fix.yaml",
        target_name="fixtest/test1.py",
        options=["--test"],
        output_format=OutputFormat.TEXT,
    )

    snapshot.assert_match(
        results,
        "output.txt",
    )


# It should report passing fixtests in the JSON output.
# TODO: rename test_passed_json_output
@pytest.mark.kinda_slow
def test_fixtest_test1_json(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/fixtest/basic_fix.yaml",
        target_name="fixtest/test1.py",
        options=["--test"],
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(stdout, "test-results.json")


# It should report no tests for fixes was found in the text output.
# TODO: rename test_no_fixtest_text_output
@pytest.mark.kinda_slow
def test_fixtest_test2_no_json(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/fixtest/basic_fix.yaml",
        target_name="fixtest/test2.py",
        options=["--test"],
        output_format=OutputFormat.TEXT,
    )

    snapshot.assert_match(
        results,
        "output.txt",
    )


# It should report config_missing_fixtests in the JSON output.
# TODO: rename test_missing_fixtest_json_output aka test_config_missing_fixtests
@pytest.mark.kinda_slow
def test_fixtest_test2_json(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/fixtest/basic_fix.yaml",
        target_name="fixtest/test2.py",
        options=["--test"],
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(stdout, "test-results.json")


# It should show a diff when a fixtest does not pass in the text output.
# TODO: rename test_fixtest_not_passed_show_diff
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_fixtest_test3_no_json(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/fixtest/other_fix.yaml",
        target_name="fixtest/test3.py",
        options=["--test"],
        output_format=OutputFormat.TEXT,
        assert_exit_code=1,
    )

    snapshot.assert_match(
        results,
        "output.txt",
    )


# It should report '"passed": false' for a bad fixtest in the JSON output.
# TODO: rename test_fixtest_not_passed_json_output
@pytest.mark.kinda_slow
def test_fixtest_test3_json(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/fixtest/other_fix.yaml",
        target_name="fixtest/test3.py",
        options=["--test"],
        output_format=OutputFormat.JSON,
        assert_exit_code=1,
    )
    snapshot.assert_match(stdout, "test-results.json")


# It should report failing match and failing fixtest in the text output.
# TODO: rename test_fixtest_not_matched_text_output
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_fixtest_test4_no_json(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results = run_semgrep_in_tmp(
        "rules/fixtest/other_pattern.yaml",
        target_name="fixtest/test4.py",
        options=["--test"],
        output_format=OutputFormat.TEXT,
        assert_exit_code=1,
        use_click_runner=True,  # TODO: does not seem related to mocking but still fail with False
    )
    snapshot.assert_match(
        results.as_snapshot(
            mask=[re.compile(r"test file path: (.+?)/fixtest/test4.py")]
        ),
        "results.txt",
    )


# It should report failing match and fixtest in the JSON output.
# TODO: rename test_fixtest_not_matched_json_output
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_fixtest_test4_json(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results = run_semgrep_in_tmp(
        "rules/fixtest/other_pattern.yaml",
        target_name="fixtest/test4.py",
        options=["--test"],
        output_format=OutputFormat.JSON,
        assert_exit_code=1,
        use_click_runner=True,  # TODO: does not seem related to mocking but still fail with False
    )
    snapshot.assert_match(
        results.as_snapshot(
            mask=[re.compile(r"test file path: (.+?)/fixtest/test4.py")]
        ),
        "results.txt",
    )


# It should report when a rule does not have a corresponding test file.
# TODO: rename test_no_test_or_fixtest_found_text_output
# TODO? pysemgrep test should really report an error actually when run
# with an inexistent file
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_fixtest_test5_no_json(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/fixtest/basic_fix.yaml",
        target_name="fixtest/inexistent.py",
        options=["--test"],
        output_format=OutputFormat.TEXT,
    )

    snapshot.assert_match(
        results,
        "output.txt",
    )


# It should remove a config_missing_tests and config_missing_fixtests
# in the JSON output.
# TODO: rename test_no_test_or_fixtest_found_json_output
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_fixtest_test5_json(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/fixtest/basic_fix.yaml",
        target_name="fixtest/inexistent.py",
        options=["--test"],
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(stdout, "test-results.json")


# It should report config_missing_fixtest for rules containing a fix-regex:
# at whatever position (not just the first rule), and without an associated
# target.fixed.ext file.
@pytest.mark.kinda_slow
def test_missing_fixtest_fix_regex(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/fixtest/basic_fix_regex.yaml",
        target_name="fixtest/no_associated_fixed.py",
        options=["--test"],
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(stdout, "test-results.json")


# It should not add the trailing newlines from a fix: replacement string
# in the fixed file.
# Note that this matters only for languages where the AST-based autofix
# is not supported (e.g., Go); Indeed, with AST-based autofix semgrep-core
# is parsing the fix pattern and then pretty print back the transformed
# pattern, so newlines do not matter.
@pytest.mark.kinda_slow
def test_fix_trailing_newline(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/fixtest/fix_trailing_newline.yaml",
        target_name="fixtest/basic.go",
        options=["--test"],
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(stdout, "test-results.json")
