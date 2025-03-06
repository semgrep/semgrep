# These tests used to be in test_exclude_include but they're mostly about
# formatting results when files are skipped.
import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


# Test output formatting in verbose mode.
#
# Exclude all the files with '--exclude' patterns resulting in 0 files being
# scanned.
@pytest.mark.kinda_slow
@pytest.mark.parametrize("use_semgrepignore_v2", [True, False], ids=["v2", "v1"])
@pytest.mark.osemfail
def test_exclude_include_verbose_sorted_1(
    run_semgrep_on_copied_files: RunSemgrep, snapshot, use_semgrepignore_v2
):
    snapshot.assert_match(
        run_semgrep_on_copied_files(
            "rules/eqeq.yaml",
            options=["--exclude", "excluded.*", "--exclude", "included.*", "--verbose"],
            output_format=OutputFormat.TEXT,
            target_name="exclude_include",
            assert_exit_code=None,
            use_semgrepignore_v2=use_semgrepignore_v2,
        ).stderr,
        "results.err",
    )


# Another test for output formatting in verbose mode.
#
# Exclude all the files with '--exclude' patterns resulting in 0 files being
# scanned.
@pytest.mark.kinda_slow
@pytest.mark.parametrize("use_semgrepignore_v2", [True, False], ids=["v2", "v1"])
@pytest.mark.osemfail
def test_exclude_include_verbose_sorted_2(
    run_semgrep_on_copied_files: RunSemgrep, snapshot, use_semgrepignore_v2
):
    snapshot.assert_match(
        run_semgrep_on_copied_files(
            "rules/nosem.yaml",
            options=["--exclude", "*.*", "--verbose"],
            output_format=OutputFormat.TEXT,
            target_name="basic",
            assert_exit_code=None,
            use_semgrepignore_v2=use_semgrepignore_v2,
        ).stderr,
        "results.err",
    )
