import pytest
from tests.conftest import mask_times
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


# NOTE: We do NOT check the stack size (ulimit -s) ever, since with ocaml 5
# everything is on the heap essentially, even the stack. So we'd never run into
# a ulimit issue
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
