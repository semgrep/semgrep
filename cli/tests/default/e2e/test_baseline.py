import os
import subprocess
import tempfile
import textwrap
from collections import defaultdict
from itertools import permutations
from pathlib import Path
from typing import Optional

import pytest
from tests.semgrep_runner import SEMGREP_BASE_SCAN_COMMAND

pytestmark = pytest.mark.kinda_slow

SENTINEL_1 = 23478921


def _git_commit(serial_no: int = 1, add: bool = False) -> str:
    if add:
        subprocess.run(
            ["git", "add", "."],
            check=True,
            capture_output=True,
        )
    year = 2000 + serial_no  # arbitrary base year
    date_string = f"Mon 10 Mar {year} 00:00:00Z"

    subprocess.run(
        [
            "git",
            "-c",
            "user.name=Py Test",
            "-c",
            "user.email=py@test.me",
            "commit",
            "--allow-empty",
            "-m",
            f"commit #{serial_no}",
            "--date",
            date_string,
        ],
        env={"GIT_COMMITTER_DATE": date_string},
        check=True,
        capture_output=True,
    )
    return subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()


def _git_merge(ref: str) -> str:
    date_string = f"Mon 10 Mar 2000 00:00:00Z"

    subprocess.run(
        [
            "git",
            "-c",
            "user.name=Py Test",
            "-c",
            "user.email=py@test.me",
            "merge",
            "--allow-unrelated-histories",
            ref,
            "-m",
            f"merging {ref}",
        ],
        env={"GIT_COMMITTER_DATE": date_string},
        check=True,
        capture_output=True,
    )
    subprocess.run(
        [
            "git",
            "-c",
            "user.name=Py Test",
            "-c",
            "user.email=py@test.me",
            "commit",
            "--amend",
            "--no-edit",
            "--date",
            date_string,
        ],
        env={"GIT_COMMITTER_DATE": date_string},
        check=True,
        capture_output=True,
    )
    return subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()


def run_sentinel_scan(check: bool = True, base_commit: Optional[str] = None):
    env = {"LANG": "en_US.UTF-8"}
    env["SEMGREP_USER_AGENT_APPEND"] = "testing"
    unique_settings_file = tempfile.NamedTemporaryFile().name
    Path(unique_settings_file).write_text(
        "anonymous_user_id: 5f52484c-3f82-4779-9353-b29bbd3193b6\n"
        "has_shown_metrics_notification: true\n"
    )
    env["SEMGREP_SETTINGS_FILE"] = unique_settings_file
    env["PATH"] = os.environ.get("PATH", "")

    cmd = SEMGREP_BASE_SCAN_COMMAND + [
        "--disable-version-check",
        "--metrics",
        "off",
        "-e",
        f"$X = {SENTINEL_1}",
        "-l",
        "python",
    ]
    if base_commit:
        cmd.extend(["--baseline-commit", base_commit])

    try:
        return subprocess.run(
            cmd,
            capture_output=True,
            encoding="utf-8",
            check=check,
            env=env,
        )
    except subprocess.CalledProcessError as e:
        print("STDOUT from sentinel scan subprocess:")
        print(e.output)
        print("STDERR from sentinel scan subprocess:")
        print(e.stderr)
        raise e


def assert_out_match(snapshot, output, snapshot_name):
    return snapshot.assert_match(textwrap.dedent(output.stdout), snapshot_name)


def assert_err_match(snapshot, output, snapshot_name, replace_base_commit=None):
    if "PYTEST_USE_OSEMGREP" in os.environ:
        # PySemgrep and OSemgrep currently display slightly different information
        # messages, causing a mismatch in the stderr output. While we may consider
        # activating this assertion for OSemgrep in the future, it will only be
        # feasible once PySemgrep and OSemgrep exhibit identical line-by-line
        # information messages.
        return
    err = (
        output.stderr.replace(replace_base_commit, "baseline-commit")
        if replace_base_commit
        else output.stderr
    )
    return snapshot.assert_match(textwrap.dedent(err), snapshot_name)


def test_one_commit_with_baseline(git_tmp_path, snapshot):
    foo = git_tmp_path / "foo.py"
    foo.write_text(f"x = {SENTINEL_1}\n")
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")

    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Add and commit noop change
    _git_commit(2)
    # Non-baseline scan should report findings
    output = run_sentinel_scan()
    assert_out_match(snapshot, output, "output.txt")
    assert (
        output.stdout != ""
    )  # If you fail this assertion means above snapshot was incorrectly changed
    assert_err_match(snapshot, output, "error.txt")

    # Baseline scan should report 0 findings
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    assert baseline_output.stdout == ""
    assert_err_match(
        snapshot, baseline_output, "baseline_error.txt", replace_base_commit=base_commit
    )


def test_symlink(git_tmp_path, snapshot):
    # Test that head having no change to base (git commit --allow-empty)
    # doesnt break semgrep
    foo = git_tmp_path / "foo.py"
    foo.write_text(f"x = {SENTINEL_1}\n")
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")
    bar_link = git_tmp_path / "bar_link.py"
    bar_link.symlink_to(bar)
    bar_link_link = git_tmp_path / "bar_link_link.py"
    bar_link_link.symlink_to(bar_link)
    broken_link = git_tmp_path / "broken_link.py"
    broken_link.symlink_to("broken")

    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Add and commit noop change
    _git_commit(2)
    # Non-baseline scan should report findings
    output = run_sentinel_scan()
    assert_out_match(snapshot, output, "output.txt")
    assert (
        output.stdout != ""
    )  # If you fail this assertion means above snapshot was incorrectly changed
    assert_err_match(snapshot, output, "error.txt")

    # Baseline scan should report 0 findings
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    assert baseline_output.stdout == ""
    assert_err_match(
        snapshot, baseline_output, "baseline_error.txt", replace_base_commit=base_commit
    )


def test_renamed_dir(git_tmp_path, snapshot):
    dir = git_tmp_path / "dir_old"
    dir.mkdir()
    foo = dir / "foo.py"
    foo.write_text(f"x = {SENTINEL_1}\n")
    bar = dir / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")
    _git_commit(1, add=True)

    dir.rename(git_tmp_path / "dir_new")
    _git_commit(2, add=True)

    # Non-baseline scan should report findings
    output = run_sentinel_scan()
    assert_out_match(snapshot, output, "full.out")
    assert (
        output.stdout != ""
    ), "If you fail this assertion, above snapshot was incorrectly changed"
    assert_err_match(snapshot, output, "full.err")

    # Diff scan should report no findings due to
    # renamed file detection and diffing logic
    baseline_output = run_sentinel_scan(base_commit="HEAD^")
    assert_out_match(snapshot, baseline_output, "diff.out")
    assert_err_match(snapshot, baseline_output, "diff.err")


def test_dir_symlink_changed(git_tmp_path, snapshot):
    dir_one = git_tmp_path / "dir_one"
    dir_two = git_tmp_path / "dir_two"
    dir_one.mkdir()
    dir_two.mkdir()
    dir_link = git_tmp_path / "dir_link"
    dir_link.symlink_to(dir_one)

    foo = dir_one / "foo.py"
    foo.write_text(f"x = {SENTINEL_1}\n")
    bar = dir_two / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")
    _git_commit(1, add=True)

    dir_link.unlink()
    dir_link.symlink_to(dir_two)
    _git_commit(2, add=True)

    # Non-baseline scan should report findings
    output = run_sentinel_scan()
    assert_out_match(snapshot, output, "full.out")
    assert (
        output.stdout != ""
    ), "If you fail this assertion, above snapshot was incorrectly changed"
    assert_err_match(snapshot, output, "full.err")

    # Baseline scan should report no findings
    baseline_output = run_sentinel_scan(base_commit="HEAD^")
    assert_out_match(snapshot, baseline_output, "diff.out")
    assert_err_match(snapshot, baseline_output, "diff.err")


def test_file_changed_to_dir(git_tmp_path, snapshot):
    file_or_dir_path = git_tmp_path / "file_or_dir.py"
    file_or_dir_path.write_text(f"x = {SENTINEL_1}\n")
    _git_commit(1, add=True)

    file_or_dir_path.unlink()
    file_or_dir_path.mkdir()

    foo = file_or_dir_path / "foo.py"
    foo.write_text(f"x = {SENTINEL_1}\n")
    bar = file_or_dir_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")
    _git_commit(2, add=True)

    # Non-baseline scan should report findings
    output = run_sentinel_scan()
    assert_out_match(snapshot, output, "full.out")
    assert (
        output.stdout != ""
    ), "If you fail this assertion, above snapshot was incorrectly changed"
    assert_err_match(snapshot, output, "full.err")

    # Diff scan should report no findings
    baseline_output = run_sentinel_scan(base_commit="HEAD^")
    assert_out_match(snapshot, baseline_output, "diff.out")
    assert_err_match(snapshot, baseline_output, "diff.err")


def test_dir_changed_to_file(git_tmp_path, snapshot):
    file_or_dir_path = git_tmp_path / "file_or_dir.py"
    file_or_dir_path.mkdir()

    foo = file_or_dir_path / "foo.py"
    foo.write_text(f"x = {SENTINEL_1}\n")
    bar = file_or_dir_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")

    _git_commit(1, add=True)

    foo.unlink()
    bar.unlink()
    file_or_dir_path.rmdir()
    file_or_dir_path.write_text(f"x = {SENTINEL_1}\n")
    _git_commit(2, add=True)

    # Non-baseline scan should report findings
    output = run_sentinel_scan()
    assert_out_match(snapshot, output, "full.out")
    assert (
        output.stdout != ""
    ), "If you fail this assertion, above snapshot was incorrectly changed"
    assert_err_match(snapshot, output, "full.err")

    # Diff scan should report no findings
    baseline_output = run_sentinel_scan(base_commit="HEAD^")
    assert_out_match(snapshot, baseline_output, "diff.out")
    assert_err_match(snapshot, baseline_output, "diff.err")


def test_no_findings_both(git_tmp_path, snapshot):
    # Test if no findings in head or base semgrep doesnt explode
    foo = git_tmp_path / "foo.py"
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = 1\n")
    foo.write_text(f"x = 1\n")

    # Add files with no finding
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Add files with no finding
    baz = git_tmp_path / "baz.py"
    baz.write_text("z = 1")
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(2)

    # Non-baseline scan should report no findings
    output = run_sentinel_scan()
    assert output.stdout == ""
    assert_err_match(snapshot, output, "error.txt", replace_base_commit=base_commit)

    # Baseline scan should report no findings
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    assert baseline_output.stdout == output.stdout
    assert_err_match(
        snapshot, baseline_output, "baseline_error.txt", replace_base_commit=base_commit
    )


def test_file_changed_to_symlink(git_tmp_path, snapshot):
    file_or_dir_path = git_tmp_path / "file_or_link.py"
    file_or_dir_path.write_text(f"x = {SENTINEL_1}\n")
    _git_commit(1, add=True)

    file_or_dir_path.rename("definitely_a_file.py")
    file_or_dir_path.symlink_to("definitely_a_file.py")
    _git_commit(2, add=True)

    # Non-baseline scan should report findings
    output = run_sentinel_scan()
    assert_out_match(snapshot, output, "full.out")
    assert (
        output.stdout != ""
    ), "If you fail this assertion, above snapshot was incorrectly changed"
    assert_err_match(snapshot, output, "full.err")

    baseline_output = run_sentinel_scan(base_commit="HEAD^")
    assert_out_match(snapshot, baseline_output, "diff.out")
    assert_err_match(snapshot, baseline_output, "diff.err")


def test_symlink_changed_to_file(git_tmp_path, snapshot):
    file_path = git_tmp_path / "definitely_a_file.py"
    file_path.write_text(f"x = {SENTINEL_1}\n")
    symlink_or_file_path = git_tmp_path / "symlink_or_file.py"
    symlink_or_file_path.symlink_to("definitely_a_file.py")
    _git_commit(1, add=True)

    symlink_or_file_path.unlink()
    file_path.rename(symlink_or_file_path)
    _git_commit(2, add=True)

    # Non-baseline scan should report findings
    output = run_sentinel_scan()
    assert_out_match(snapshot, output, "full.out")
    assert (
        output.stdout != ""
    ), "If you fail this assertion, above snapshot was incorrectly changed"
    assert_err_match(snapshot, output, "full.err")

    baseline_output = run_sentinel_scan(base_commit="HEAD^")
    assert_out_match(snapshot, baseline_output, "diff.out")
    assert_err_match(snapshot, baseline_output, "diff.err")


def test_no_findings_head(git_tmp_path, snapshot):
    # Test that no findings in head reports no findings even if
    # findings in baseline
    foo = git_tmp_path / "foo.py"
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")
    foo.write_text(f"x = {SENTINEL_1}\n")

    # Add baseline finding
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Add head finding
    baz = git_tmp_path / "baz.py"
    baz.write_text("z  = 1")
    foo.write_text("")  # Overwrite foo and baz to empty file
    bar.write_text("")
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(2)

    # Non-baseline scan should report no findings
    output = run_sentinel_scan()
    assert output.stdout == ""
    assert_err_match(snapshot, output, "error.txt", replace_base_commit=base_commit)

    # Baseline scan should report no findings
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    assert baseline_output.stdout == output.stdout
    assert_err_match(
        snapshot, baseline_output, "baseline_error.txt", replace_base_commit=base_commit
    )


def test_no_findings_baseline(git_tmp_path, snapshot):
    # Test when head contains all findings and baseline doesnt contain any
    foo = git_tmp_path / "foo.py"
    foo.write_text(f"x = 1")

    # Add baseline finding
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Add head finding
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")
    foo.write_text(f"x = {SENTINEL_1}\n")
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(2)

    # Non-baseline scan should report findings
    output = run_sentinel_scan()
    assert_out_match(snapshot, output, "output.txt")
    assert output.stdout != ""
    assert_err_match(snapshot, output, "error.txt", replace_base_commit=base_commit)

    # Baseline scan should report same findings
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    assert_out_match(snapshot, baseline_output, "baseline_output.txt")
    assert baseline_output.stdout == output.stdout
    assert_err_match(
        snapshot, baseline_output, "baseline_error.txt", replace_base_commit=base_commit
    )


def test_some_intersection(git_tmp_path, snapshot):
    # Test when baseline contains some findings of head
    foo = git_tmp_path / "foo.py"
    foo.write_text(f"x = {SENTINEL_1}\n")

    # Add baseline finding
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Add head finding
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(2)

    # Non-baseline scan should report 2 findings
    output = run_sentinel_scan()
    assert_out_match(snapshot, output, "output.txt")
    assert output.stdout != ""
    assert_err_match(snapshot, output, "error.txt", replace_base_commit=base_commit)

    # Baseline scan should report 1 finding but hide 1
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    assert_out_match(snapshot, baseline_output, "baseline_output.txt")
    assert baseline_output.stdout != output.stdout
    assert_err_match(
        snapshot, baseline_output, "baseline_error.txt", replace_base_commit=base_commit
    )


def test_all_intersect(git_tmp_path, snapshot):
    # Test when baseline and head contain same findings none are reported
    foo = git_tmp_path / "foo.py"
    foo.write_text(f"x = {SENTINEL_1}\n")

    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")

    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Add and commit noop change
    foo.write_text(foo.read_text() + "z = 1\n")  # Note write_text overwrites
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(2)

    # Non-baseline scan should report findings
    output = run_sentinel_scan()
    assert_out_match(snapshot, output, "output.txt")
    assert (
        output.stdout != ""
    )  # If you fail this assertion means above snapshot was incorreclty changed
    assert_err_match(snapshot, output, "error.txt")

    # Baseline scan should report 0 findings
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    assert baseline_output.stdout == ""
    assert_err_match(
        snapshot, baseline_output, "baseline_error.txt", replace_base_commit=base_commit
    )


def test_no_intersection(git_tmp_path, snapshot):
    # If no intersection of baseline and head finding should still report head finding
    foo = git_tmp_path / "foo.py"
    foo.write_text(f"x = {SENTINEL_1}")

    # Add baseline finding
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Add head finding remove baseline finding
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")
    foo.write_text("")  # Overwrite foo
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(2)

    # Non-baseline scan should report 1 finding
    output = run_sentinel_scan()
    assert_out_match(snapshot, output, "output.txt")
    assert output.stdout != ""
    assert_err_match(snapshot, output, "error.txt", replace_base_commit=base_commit)

    # Baseline scan should report same finding
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    assert baseline_output.stdout == output.stdout
    assert_err_match(
        snapshot, baseline_output, "baseline_error.txt", replace_base_commit=base_commit
    )


@pytest.mark.parametrize(
    "new_name",
    [
        pytest.param("bar.py", id="case-insensitive"),
        pytest.param("Foo.py", id="case-sensitive"),
    ],
)
def test_renamed_file(git_tmp_path, snapshot, new_name):
    old_name = "foo.py"
    old_path = git_tmp_path / old_name
    # write lots of static text so git will recognize the file as renamed
    old_path.write_text("1\n\n" * 100 + f"x = {SENTINEL_1}")
    base_commit = _git_commit(1, add=True)

    subprocess.run(
        ["git", "mv", old_name, new_name],
        check=True,
        capture_output=True,
    )
    new_path = git_tmp_path / new_name
    new_path.write_text("1\n\n" * 100 + f"x = {SENTINEL_1}\n\n" + f"y = {SENTINEL_1}")
    _git_commit(2, add=True)

    # Non-baseline scan should report 1 finding
    output = run_sentinel_scan()
    assert_out_match(snapshot, output, "output.txt")
    assert output.stdout != ""
    assert_err_match(snapshot, output, "error.txt", replace_base_commit=base_commit)

    # Diff scan should report no findings
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    assert_out_match(snapshot, baseline_output, "baseline_output.txt")
    assert_err_match(
        snapshot, baseline_output, "baseline_error.txt", replace_base_commit=base_commit
    )

    assert set(git_tmp_path.glob("*.py")) == {
        new_path
    }, "the old path should be gone now"


# TODO
# def test_multiple_on_same_line(git_tmp_path, snapshot):
#    pass
#
#
# TODO
# def test_run_in_subdirectory(git_tmp_path, snapshot):
#    pass


def test_unstaged_changes(git_tmp_path, snapshot):
    # Should not abort if have unstaged changes
    foo = git_tmp_path / "foo"
    foo.mkdir()
    foo_a = foo / "a.py"
    foo_a.touch()
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    foo_a.write_text(f"y = {SENTINEL_1}\n")
    output = run_sentinel_scan(base_commit=base_commit, check=False)
    assert output.returncode == 0
    assert_out_match(snapshot, output, "output.txt")
    assert_err_match(snapshot, output, "error.txt")


def test_staged_changes(git_tmp_path, snapshot):
    # Should report findings in staged changes

    # Should not abort if have unstaged changes
    foo = git_tmp_path / "foo"
    foo.mkdir()
    foo_a = foo / "a.py"
    foo_a.touch()
    foo_a.write_text(f"y = {SENTINEL_1}\n")
    subprocess.run(["git", "add", "."], check=True, capture_output=True)

    _git_commit(1)

    foo_a.write_text(f"y = {SENTINEL_1}\nx = {SENTINEL_1}")
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    output = run_sentinel_scan(base_commit=base_commit, check=False)
    assert output.returncode == 0
    assert_out_match(snapshot, output, "output.txt")
    assert_err_match(snapshot, output, "error.txt")


# TODO
# def test_baseline_has_head_untracked(git_tmp_path, snapshot):
#    pass


def test_not_git_directory(monkeypatch, tmp_path, snapshot):
    # Should abort baseline scan if not a git directory
    monkeypatch.chdir(tmp_path)
    foo = tmp_path / "foo"
    foo.mkdir()
    foo_a = foo / "a.py"
    foo_a.write_text("y = 55555555\n")

    output = run_sentinel_scan(base_commit="12345", check=False)
    assert output.returncode != 0
    # TODO: the assert_match below is not reliable across different versions of git
    # Maybe we should just assert that the string 'fatal: not a git repository'
    # is in the error ouput. The rest can vary a lot (especially the indentation)
    # snapshot.assert_match(output.stderr, "error.txt")


def test_commit_doesnt_exist(git_tmp_path, snapshot):
    # Should abort baseline scan if baseline is not valid commit
    foo = git_tmp_path / "foo"
    foo.mkdir()
    foo_a = foo / "a.py"
    foo_a.touch()
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(1)

    output = run_sentinel_scan(base_commit="12345", check=False)
    assert output.returncode != 0
    # Don't snapshot stderr because it contains a stack trace that
    # shows different paths to the source code depending on the host.


@pytest.fixture
def complex_merge_repo(git_tmp_path, snapshot):
    r"""
    This generates a complex history like this:

    $ git log --graph --oneline --decorate
    * deb1e5a (HEAD -> many-merges) commit #9
    * e4075c1 commit #8
    * 69400f4 commit #7
    * 836ad06 commit #6
    * 4f04b2a commit #5
    * 1bd9089 commit #4
    * a78685a commit #3
    * dd9b858 commit #2
    * 3ee83bc commit #1
    *   9f1bffc Merge commit '4a372020118b6787c208de20e374aac0cdc2b841' into many-merges
    |\
    | * 4a37202 commit #7
    | * 5fe55c3 commit #6
    | * f0d065e commit #5
    * |   26b9bb4 Merge commit '657cdbabbd73778d81796d0128207fee79136f10' into many-merges
    |\ \
    | * | 657cdba commit #5
    | * | 4c7d0ba commit #4
    * | |   0437d72 Merge commit 'b62d8d638ebfd9494bd16634cf8783db074836ea' into many-merges
    |\ \ \
    | | |/
    | |/|
    | * | b62d8d6 commit #4
    | * | 97acb1c commit #3
    | * | 55e425e commit #2
    * | |   090c7e2 Merge commit '5275d4bb21ad3fd0c6361b4e252b7d46ce3a6583' into many-merges
    |\ \ \
    | |/ /
    |/| /
    | |/
    | * 5275d4b commit #3
    | * 9121d4d commit #2
    * | e554aec commit #1
    |/
    * 384e83e commit #1
    """
    commits = defaultdict(list)
    foo = git_tmp_path / "foo.py"
    bar = git_tmp_path / "bar.py"
    baz = git_tmp_path / "baz.py"

    subprocess.run(["git", "checkout", "-b", "foo"])
    for index in range(1, 10):
        foo.open("a").write(f"foo = {SENTINEL_1}\n\n")
        commits["foo"].append(_git_commit(index, add=True))

    subprocess.run(["git", "checkout", commits["foo"][0]])
    subprocess.run(["git", "checkout", "-b", "bar"])

    for index in range(1, 10):
        bar.open("a").write(f"bar = {SENTINEL_1}\n\n")
        commits["bar"].append(_git_commit(index, add=True))

    subprocess.run(["git", "checkout", "foo"])
    _git_merge("bar~6")

    subprocess.run(["git", "checkout", commits["foo"][0]])
    subprocess.run(["git", "checkout", "-b", "baz"])
    for foo_commit, bar_commit in zip(commits["foo"][::2], commits["bar"][::3]):
        _git_merge(foo_commit)
        _git_merge(bar_commit)

    for index in range(1, 10):
        baz.open("a").write(f"baz = {SENTINEL_1}\n\n")
        commits["baz"].append(_git_commit(index, add=True))


@pytest.mark.parametrize("current, baseline", permutations(["foo", "bar", "baz"], 2))
def test_crisscrossing_merges(complex_merge_repo, current, baseline, snapshot):
    subprocess.run(["git", "checkout", current])
    output = run_sentinel_scan(base_commit=baseline)
    assert_out_match(snapshot, output, f"stdout.txt")
    assert_err_match(snapshot, output, f"stderr.txt")


def test_conflicting_file_and_main_branch_names(git_tmp_path, snapshot):
    # Test when a file in the root dir has the same name as the main branch
    main_file = git_tmp_path / "main"
    main_file.write_text("this is a file named 'main'\n")
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    _git_commit(1)

    # Ambiguity between the main branch and the file should not cause an error
    run_sentinel_scan(base_commit="main")
