import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


# it should not fail the whole scan for unhandled exn (e.g., r2c_was_fatal)
# that could still be attached to a file in semgrep-core
@pytest.mark.kinda_slow
def test_exit_code_warning_error(run_semgrep_in_tmp: RunSemgrep):
    stdout, stderr = run_semgrep_in_tmp(
        config=f"rules/stuff_stmt.yaml",
        target_name=f"error_management/r2c_was_fatal.py",
        options=["--verbose"],
        output_format=OutputFormat.TEXT,
        strict=False,
        assert_exit_code=0,
    )


# it should fail the scan if using --strict though
@pytest.mark.kinda_slow
def test_exit_code_with_strict(run_semgrep_in_tmp: RunSemgrep):
    stdout, stderr = run_semgrep_in_tmp(
        config=f"rules/stuff_stmt.yaml",
        target_name=f"error_management/r2c_was_fatal.py",
        options=["--verbose"],
        output_format=OutputFormat.TEXT,
        strict=True,
        assert_exit_code=2,
    )
