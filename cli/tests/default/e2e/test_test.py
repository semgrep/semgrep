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
import os
import subprocess
from pathlib import Path

import pytest
from tests.conftest import skip_on_windows
from tests.fixtures import RunSemgrep
from tests.semgrep_runner import SEMGREP_BASE_SCAN_COMMAND

from semgrep.constants import OutputFormat
from semgrep.util import IS_WINDOWS


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


# TODO: Not sure what we're testing here
# was added in https://github.com/semgrep/semgrep/pull/8427 By Austin
@pytest.mark.slow
# works correctly on Windows when run separately, but timesout when run with -n
# auto alongside other tests.
@skip_on_windows
def test_cli_test_from_entrypoint(posix_snapshot):
    env = {}
    env["PATH"] = os.environ.get("PATH", "")

    cmd = SEMGREP_BASE_SCAN_COMMAND + [
        "--test",
        "--config",
        "rules/basic.yaml",
        "targets/test_test/basic.py",
    ]
    if IS_WINDOWS:
        # todo: remove when Windows is a supported platform
        env["SEMGREP_FORCE_INSTALL"] = "1"
        # Required env vars to be able to run semgrep
        env["SYSTEMROOT"] = os.environ.get("SYSTEMROOT", "")
        env["USERPROFILE"] = os.environ.get("USERPROFILE", "")
    result = subprocess.run(
        cmd,
        cwd=Path(__file__).parent,
        capture_output=True,
        encoding="utf-8",
        check=True,
        env=env,
        timeout=15,
    )
    posix_snapshot.assert_match(result.stdout, "output.txt")


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
