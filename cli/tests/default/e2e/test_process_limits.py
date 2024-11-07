import subprocess
from pathlib import Path

import pytest
from tests.conftest import mask_times
from tests.fixtures import RunSemgrep
from tests.semgrep_runner import SEMGREP_BASE_SCAN_COMMAND_STR

from semgrep.constants import OutputFormat


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_max_memory(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/long.yaml",
        options=["--verbose", "--max-memory", "1"],
        target_name="equivalence",
        strict=False,
    )
    snapshot.assert_match(mask_times(stdout), "results.json")
    snapshot.assert_match(stderr, "error.txt")


@pytest.mark.osemfail
@pytest.mark.slow
def test_stack_size(run_semgrep_in_tmp: RunSemgrep, snapshot):
    """
    Verify that semgrep raises the soft stack limit if possible
    when calling semgrep core
    """

    # long.yaml and equivalence were chosen since they happen to cause
    # stack exhaustion
    e2e_dir = Path(__file__).parent
    targetpath = Path(e2e_dir / "targets").resolve() / "equivalence"
    rulepath = Path(e2e_dir / "rules").resolve() / "long.yaml"

    # Set the hard as well as the soft stack limit. This should force a stack
    # overflow. If this fails, the test is broken and needs to be fixed.
    # Do not just delete this assertion. It means the actual test below does
    # not accurately verify that we are solving the stack exhaustion

    # NOTE: This test requires some tuning to work on different systems
    # as we can see a SIGSEGV on macOS and Linux for too small of ulimit sizes.
    # For too high of ulimit sizes, we will not see a stack overflow.
    ulimit_size = 384

    # '--x-ignore-semgrepignore-files' is needed since our repo has a
    # .semgrepignore file that excludes '/tests/'.
    output = subprocess.run(
        f"ulimit -s {ulimit_size} && {SEMGREP_BASE_SCAN_COMMAND_STR} --x-ignore-semgrepignore-files --disable-version-check --metrics off --config {rulepath} --jobs 1 --verbose {targetpath}",
        shell=True,
        capture_output=True,
        encoding="utf-8",
    )
    print(output.stdout)
    print(output.stderr)
    assert (
        "semgrep-core exit code: -11" in output.stderr
        or "Stack overflow" in output.stderr
    )

    # If only set soft limit, semgrep should raise it as necessary so we don't hit soft limit
    output = subprocess.run(
        f"ulimit -S -s {ulimit_size} && {SEMGREP_BASE_SCAN_COMMAND_STR} --disable-version-check --metrics off --config {rulepath} --jobs 1 --verbose {targetpath}",
        shell=True,
        capture_output=True,
        encoding="utf-8",
    )
    print(output.stderr)
    # with a soft limit, semgrep should terminate without errors
    assert "semgrep-core exit code: -11" not in output.stderr
    assert "Stack overflow" not in output.stderr
    assert output.returncode == 0


@pytest.mark.slow
def test_timeout(run_semgrep_in_tmp: RunSemgrep, snapshot):
    # Check that semgrep-core timeouts are properly handled

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/long.yaml",
            options=["--timeout", "1"],
            target_name="equivalence",
            strict=False,
        ).stdout,
        "results.json",
    )


@pytest.mark.osemfail
@pytest.mark.slow
def test_timeout_threshold(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results = run_semgrep_in_tmp(
        "rules/multiple-long.yaml",
        options=["--verbose", "--timeout", "1", "--timeout-threshold", "1"],
        target_name="equivalence",
        strict=False,
    ).stdout
    snapshot.assert_match(
        mask_times(results),
        "results.json",
    )

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/multiple-long.yaml",
            output_format=OutputFormat.TEXT,
            force_color=True,
            options=["--verbose", "--timeout", "1", "--timeout-threshold", "1"],
            target_name="equivalence",
            strict=False,
        ).stderr,
        "error.txt",
    )

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/multiple-long.yaml",
            output_format=OutputFormat.TEXT,
            force_color=True,
            options=["--verbose", "--timeout", "1", "--timeout-threshold", "2"],
            target_name="equivalence",
            strict=False,
        ).stderr,
        "error_2.txt",
    )


@pytest.mark.osemfail
@pytest.mark.slow
def test_spacegrep_timeout(run_semgrep_in_tmp: RunSemgrep, snapshot):
    # Check that spacegrep timeouts are handled gracefully.
    #
    # The pattern is designed to defeat any optimization that would
    # prevent a timeout. Both the words 'Frob' and 'Yoyodyne' occur
    # once in the file but in a different order, preventing any match.
    #
    pattern = "$A ... $B ... $C ... Frob ... Yoyodyne"

    stdout, stderr = run_semgrep_in_tmp(
        config=None,
        target_name="spacegrep_timeout/gnu-lgplv2.txt",
        options=["--lang=generic", "--pattern", pattern, "--timeout=1"],
        output_format=OutputFormat.TEXT,
        strict=False,  # don't fail due to timeout
    )

    snapshot.assert_match(stdout, "results.json")
    snapshot.assert_match(stderr, "error.txt")
