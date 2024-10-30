import os
import sys

import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


def prepare_workspace() -> None:
    print(f"Current dir: {os.getcwd()}", file=sys.stderr)
    print("Creating files with bad permissions in the workspace!", file=sys.stderr)

    # Create file tree (assume 'targets/permissions/' already exists)
    with open("targets/permissions/unreadable_file.py", "w") as file:
        file.write("secret content\n")

    with open("targets/permissions/readable_file.py", "w") as file:
        file.write("a == a\n")

    os.mkdir("targets/permissions/unreadable_subdir")

    with open("targets/permissions/unreadable_subdir/file.py", "w") as file:
        file.write("b == b\n")

    # Make some nodes unreadable
    os.chmod("targets/permissions/unreadable_subdir", 0o000)
    os.chmod("targets/permissions/unreadable_file.py", 0o000)
    os.system("ls -l targets/permissions >&2")


# Restore permissions so that the function that cleans up the workspace
# doesn't fail
def teardown_workspace() -> None:
    print(f"Current dir: {os.getcwd()}", file=sys.stderr)
    print("Restoring good permissions to allow cleanup!", file=sys.stderr)
    os.chmod("targets/permissions/unreadable_subdir", 0o700)
    os.chmod("targets/permissions/unreadable_file.py", 0o600)


# Check that target files lacking read permission are ignored gracefully.
#
# Pysemgrep fails to ignore the target without read permissions and passes
# it to semgrep-core. This is why --x-ls fails the test. These targets
# are checked again by semgrep-core which then excludes it correctly
@pytest.mark.pysemfail
@pytest.mark.kinda_slow
def test_permissions_ls(run_semgrep_on_copied_files: RunSemgrep, snapshot):
    stdout, stderr = run_semgrep_on_copied_files(
        "rules/eqeq.yaml",  # ignored by --x-ls
        options=["--x-ls"],
        output_format=OutputFormat.TEXT,
        prepare_workspace=prepare_workspace,
        target_name="permissions",
        teardown_workspace=teardown_workspace,
    )
    snapshot.assert_match(stdout, "files.list")


# End-to-end test. This exercises a regular semgrep scan, in particular
# one using semgrep-core underneath.
#
# The '--verbose' option causes the list of skipped targets to be included
# in the JSON output. It differs between git and non-git projects due to
# git doing some filtering of its own. Here, we run semgrep on a non-git
# project and we expect all skipped targets to be reported with osemgrep
# and pysemgrep.
#
def run_test_permissions_scan_full(
    run_semgrep_on_copied_files: RunSemgrep, snapshot, verbose: bool
):
    options = None
    if verbose:
        options = ["--verbose"]
    stdout, stderr = run_semgrep_on_copied_files(
        "rules/eqeq.yaml",
        options=options,
        output_format=OutputFormat.JSON,
        prepare_workspace=prepare_workspace,
        target_name="permissions",
        teardown_workspace=teardown_workspace,
    )
    snapshot.assert_match(stdout, "results.json")


# pysemgrep fails because it doesn't report the skipped folder. This is benign.
#
@pytest.mark.pysemfail
@pytest.mark.kinda_slow
def test_permissions_scan_full_strict(
    run_semgrep_on_copied_files: RunSemgrep, snapshot
):
    run_test_permissions_scan_full(run_semgrep_on_copied_files, snapshot, verbose=True)


# Less strict: don't care about whether skipped files or folders are reported.
#
@pytest.mark.kinda_slow
def test_permissions_scan_full_lax(run_semgrep_on_copied_files: RunSemgrep, snapshot):
    run_test_permissions_scan_full(run_semgrep_on_copied_files, snapshot, verbose=False)


# TODO: test on a git repo
