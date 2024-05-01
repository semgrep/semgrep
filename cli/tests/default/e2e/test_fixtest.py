# Testing the fixtest feature.
# See https://semgrep.dev/docs/writing-rules/testing-rules/#testing-autofix
#
# TODO:
#  - rename those test_fixtest_testx_xx_json; give meaningful names so we
#    know what they test and adjust also accordingly the testx.py
#  - add test to detect missing_config_fixtest when rule use fix-regex
import re

import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


# TODO: rename test_passed_text_output
@pytest.mark.kinda_slow
@pytest.mark.osemfail
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


# TODO: rename test_passed_json_output
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_fixtest_test1_json(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/fixtest/basic_fix.yaml",
        target_name="fixtest/test1.py",
        options=["--test"],
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(stdout, "test-results.json")


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


# TODO: rename test_missing_fixtest_json_output aka test_config_missing_fixtests
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_fixtest_test2_json(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/fixtest/basic_fix.yaml",
        target_name="fixtest/test2.py",
        options=["--test"],
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(stdout, "test-results.json")


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


# TODO: rename test_fixtest_not_passed_json_output
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_fixtest_test3_json(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/fixtest/other_fix.yaml",
        target_name="fixtest/test3.py",
        options=["--test"],
        output_format=OutputFormat.JSON,
        assert_exit_code=1,
    )
    snapshot.assert_match(stdout, "test-results.json")


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
