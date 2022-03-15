import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Optional

import pytest

SENTINEL_1 = 23478921


def _git_commit(serial_no: int = 1):
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
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )


def run_sentinel_scan(check: bool = True, base_commit: Optional[str] = None):
    env = {"LANG": "en_US.UTF-8"}
    env["SEMGREP_USER_AGENT_APPEND"] = "testing"
    unique_settings_file = tempfile.NamedTemporaryFile().name
    Path(unique_settings_file).write_text("has_shown_metrics_notification: true")
    env["SEMGREP_SETTINGS_FILE"] = unique_settings_file

    cmd = [
        sys.executable,
        "-m",
        "semgrep",
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
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE,
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


def test_one_commit_with_baseline(git_tmp_path, snapshot):
    # Test that head having no change to base (git commit --allow-empty)
    # doesnt break semgrep
    foo = git_tmp_path / "foo.py"
    foo.write_text(f"x = {SENTINEL_1}\n")
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")

    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Add and commit noop change
    _git_commit(2)
    # Non-baseline scan should report findings
    output = run_sentinel_scan()
    snapshot.assert_match(output.stdout, "output.txt")
    assert (
        output.stdout != ""
    )  # If you fail this assertion means above snapshot was incorrectly changed
    snapshot.assert_match(output.stderr, "error.txt")

    # Baseline scan should report 0 findings
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    assert baseline_output.stdout == ""
    snapshot.assert_match(
        baseline_output.stderr.replace(base_commit, "baseline-commit"),
        "baseline_error.txt",
    )


def test_symlink(git_tmp_path, snapshot):
    pass


def test_no_findings_both(git_tmp_path, snapshot):
    # Test if no findings in head or base semgrep doesnt explode
    foo = git_tmp_path / "foo.py"
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = 1\n")
    foo.write_text(f"x = 1\n")

    # Add files with no finding
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Add files with no finding
    baz = git_tmp_path / "baz.py"
    baz.write_text("z = 1")
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    _git_commit(2)

    # Non-baseline scan should report no findings
    output = run_sentinel_scan()
    assert output.stdout == ""
    snapshot.assert_match(
        output.stderr.replace(base_commit, "baseline-commit"), "error.txt"
    )

    # Baseline scan should report no findings
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    assert baseline_output.stdout == output.stdout
    snapshot.assert_match(
        baseline_output.stderr.replace(base_commit, "baseline-commit"),
        "baseline_error.txt",
    )


def test_no_findings_head(git_tmp_path, snapshot):
    # Test that no findings in head reports no findings even if
    # findings in baseline
    foo = git_tmp_path / "foo.py"
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")
    foo.write_text(f"x = {SENTINEL_1}\n")

    # Add baseline finding
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Add head finding
    baz = git_tmp_path / "baz.py"
    baz.write_text("z  = 1")
    foo.write_text("")  # Overwrite foo and baz to empty file
    bar.write_text("")
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    _git_commit(2)

    # Non-baseline scan should report no findings
    output = run_sentinel_scan()
    assert output.stdout == ""
    snapshot.assert_match(
        output.stderr.replace(base_commit, "baseline-commit"), "error.txt"
    )

    # Baseline scan should report no findings
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    assert baseline_output.stdout == output.stdout
    snapshot.assert_match(
        baseline_output.stderr.replace(base_commit, "baseline-commit"),
        "baseline_error.txt",
    )


def test_no_findings_baseline(git_tmp_path, snapshot):
    # Test when head contains all findings and baseline doesnt contain any
    foo = git_tmp_path / "foo.py"
    foo.write_text(f"x = 1")

    # Add baseline finding
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Add head finding
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")
    foo.write_text(f"x = {SENTINEL_1}\n")
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    _git_commit(2)

    # Non-baseline scan should report findings
    output = run_sentinel_scan()
    snapshot.assert_match(output.stdout, "output.txt")
    assert output.stdout != ""
    snapshot.assert_match(
        output.stderr.replace(base_commit, "baseline-commit"), "error.txt"
    )

    # Baseline scan should report same findings
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    snapshot.assert_match(baseline_output.stdout, "baseline_output.txt")
    assert baseline_output.stdout == output.stdout
    snapshot.assert_match(
        baseline_output.stderr.replace(base_commit, "baseline-commit"),
        "baseline_error.txt",
    )


def test_some_intersection(git_tmp_path, snapshot):
    # Test when baseline contains some findings of head
    foo = git_tmp_path / "foo.py"
    foo.write_text(f"x = {SENTINEL_1}\n")

    # Add baseline finding
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Add head finding
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    _git_commit(2)

    # Non-baseline scan should report 2 findings
    output = run_sentinel_scan()
    snapshot.assert_match(output.stdout, "output.txt")
    assert output.stdout != ""
    snapshot.assert_match(
        output.stderr.replace(base_commit, "baseline-commit"), "error.txt"
    )

    # Baseline scan should report 1 finding but hide 1
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    snapshot.assert_match(baseline_output.stdout, "baseline_output.txt")
    assert baseline_output.stdout != output.stdout
    snapshot.assert_match(
        baseline_output.stderr.replace(base_commit, "baseline-commit"),
        "baseline_error.txt",
    )


def test_all_intersect(git_tmp_path, snapshot):
    # Test when baseline and head contain same findings none are reported
    foo = git_tmp_path / "foo.py"
    foo.write_text(f"x = {SENTINEL_1}\n")

    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")

    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Add and commit noop change
    foo.write_text(foo.read_text() + "z = 1\n")  # Note write_text overwrites
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    _git_commit(2)

    # Non-baseline scan should report findings
    output = run_sentinel_scan()
    snapshot.assert_match(output.stdout, "output.txt")
    assert (
        output.stdout != ""
    )  # If you fail this assertion means above snapshot was incorreclty changed
    snapshot.assert_match(output.stderr, "error.txt")

    # Baseline scan should report 0 findings
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    assert baseline_output.stdout == ""
    snapshot.assert_match(
        baseline_output.stderr.replace(base_commit, "baseline-commit"),
        "baseline_error.txt",
    )


def test_no_intersection(git_tmp_path, snapshot):
    # If no intersection of baseline and head finding should still report head finding
    foo = git_tmp_path / "foo.py"
    foo.write_text(f"x = {SENTINEL_1}")

    # Add baseline finding
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Add head finding remove baseline finding
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")
    foo.write_text("")  # Overwrite foo
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    _git_commit(2)

    # Non-baseline scan should report 1 finding
    output = run_sentinel_scan()
    snapshot.assert_match(output.stdout, "output.txt")
    assert output.stdout != ""
    snapshot.assert_match(
        output.stderr.replace(base_commit, "baseline-commit"), "error.txt"
    )

    # Baseline scan should report same finding
    baseline_output = run_sentinel_scan(base_commit=base_commit)
    assert baseline_output.stdout == output.stdout
    snapshot.assert_match(
        baseline_output.stderr.replace(base_commit, "baseline-commit"),
        "baseline_error.txt",
    )


def test_multiple_on_same_line(git_tmp_path, snapshot):
    pass


def test_run_in_subdirectory(git_tmp_path, snapshot):
    pass


def test_unstaged_changes(git_tmp_path, snapshot):
    # Should abort if have unstaged changes
    foo = git_tmp_path / "foo"
    foo.mkdir()
    foo_a = foo / "a.py"
    foo_a.touch()
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    _git_commit(1)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    foo_a.write_text(f"y = {SENTINEL_1}\n")
    output = run_sentinel_scan(base_commit=base_commit, check=False)
    assert output.returncode != 0
    snapshot.assert_match(output.stderr, "error.txt")


def test_baseline_has_head_untracked(git_tmp_path, snapshot):
    pass


def test_not_git_directory(monkeypatch, tmp_path, snapshot):
    # Should abort baseline scan if not a git directory
    monkeypatch.chdir(tmp_path)
    foo = tmp_path / "foo"
    foo.mkdir()
    foo_a = foo / "a.py"
    foo_a.write_text("y = 55555555\n")

    output = run_sentinel_scan(base_commit="12345", check=False)
    assert output.returncode != 0
    snapshot.assert_match(output.stderr, "error.txt")


def test_commit_doesnt_exist(git_tmp_path, snapshot):
    # Should abort baseline scan if baseline is not valid commit
    foo = git_tmp_path / "foo"
    foo.mkdir()
    foo_a = foo / "a.py"
    foo_a.touch()
    subprocess.run(
        ["git", "add", "."], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    _git_commit(1)

    output = run_sentinel_scan(base_commit="12345", check=False)
    assert output.returncode != 0
    snapshot.assert_match(output.stderr, "error.txt")


@pytest.fixture
def git_tmp_path(monkeypatch, tmp_path):
    monkeypatch.chdir(tmp_path)
    # Initialize State
    subprocess.run(
        ["git", "init"], check=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE
    )
    subprocess.run(
        ["git", "config", "user.email", "baselinetest@r2c.dev"],
        check=True,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    subprocess.run(
        ["git", "config", "user.name", "Baseline Test"],
        check=True,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    subprocess.run(
        ["git", "checkout", "-B", "main"],
        check=True,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    yield tmp_path
