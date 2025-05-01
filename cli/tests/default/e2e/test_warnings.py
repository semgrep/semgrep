# Test various warnings and error messages
import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_semgrepignore_v2_warning(
    run_semgrep_on_copied_files: RunSemgrep, posix_snapshot
):
    """Check that '--semgrepignore-v2' prints a deprecation warning"""
    posix_snapshot.assert_match(
        run_semgrep_on_copied_files(
            config="rules/eqeq.yaml",
            target_name="basic",
            output_format=OutputFormat.TEXT,
            options=["--semgrepignore-v2"],
        ).stderr,
        "results.txt",
    )
