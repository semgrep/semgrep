#
# Test target selection before any rule or language-specific filtering
#
import pytest
from tests.conftest import create_git_repo
from tests.fixtures import RunSemgrep


# Check that a variety of file and folder names are tolerated and selected
# normally by semgrep scan.
#
# Compatibility note: Unix, Windows, and MacOS have different rules for
# what's allowed as a file name. Here, we only test filenames that work
# on all platforms.
#
# At least one test file name should be quoted
# by 'git ls-files' e.g. '😊' will be shown as '"\360\237\230\212"'
#
@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "prepare_workspace", [lambda: None, create_git_repo], ids=["nongit", "git"]
)
def test_target_selection(
    run_semgrep_on_copied_files: RunSemgrep, prepare_workspace, snapshot
):
    stdout, stderr = run_semgrep_on_copied_files(
        "rules/eqeq.yaml",  # ignored by --x-ls
        options=["--x-ls"],
        prepare_workspace=prepare_workspace,
        target_name="target_selection",
    )
    snapshot.assert_match(stdout, "files.list")
