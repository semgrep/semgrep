import subprocess
import sys

import pytest

SENTINEL_1 = 23478921


def test_no_files_in_baseline():
    pass


def test_empty_commit_head():
    pass


def test_empty_commit_baseline():
    pass


def test_one_commit_with_baseline():
    # `git commit --allow-empty`
    pass


def test_symlink():
    pass


def test_no_findings_both():
    pass


def test_no_findings_head(git_tmp_path, snapshot):
    foo = git_tmp_path / "foo.py"
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")
    foo.write_text(f"x = {SENTINEL_1}\n")

    # Add baseline finding
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(["git", "commit", "-m", "first"], check=True, capture_output=True)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], text=True
    ).strip()

    # Add head finding
    baz = git_tmp_path / "baz.py"
    baz.write_text("z  = 1")
    foo.write_text("")  # Overwrite foo and baz to empty file
    bar.write_text("")
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(["git", "commit", "-m", "second"], check=True, capture_output=True)

    # Non-baseline scan should report no findings
    output = subprocess.run(
        [sys.executable, "-m", "semgrep", "-e", f"$X = {SENTINEL_1}", "-l", "python"],
        capture_output=True,
        text=True,
        check=True,
    )
    assert output.stdout == ""
    snapshot.assert_match(
        output.stderr.replace(base_commit, "baseline-commit"), "error.txt"
    )

    # Baseline scan should report no findings
    baseline_output = subprocess.run(
        [
            sys.executable,
            "-m",
            "semgrep",
            "-e",
            f"$X = {SENTINEL_1}",
            "-l",
            "python",
            "--baseline-commit",
            base_commit,
        ],
        capture_output=True,
        text=True,
        check=True,
    )
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
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(["git", "commit", "-m", "first"], check=True, capture_output=True)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], text=True
    ).strip()

    # Add head finding
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")
    foo.write_text(f"x = {SENTINEL_1}\n")
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(["git", "commit", "-m", "second"], check=True, capture_output=True)

    # Non-baseline scan should report findings
    output = subprocess.run(
        [sys.executable, "-m", "semgrep", "-e", f"$X = {SENTINEL_1}", "-l", "python"],
        capture_output=True,
        text=True,
        check=True,
    )
    snapshot.assert_match(output.stdout, "output.txt")
    assert output.stdout != ""
    snapshot.assert_match(
        output.stderr.replace(base_commit, "baseline-commit"), "error.txt"
    )

    # Baseline scan should report same findings
    baseline_output = subprocess.run(
        [
            sys.executable,
            "-m",
            "semgrep",
            "-e",
            f"$X = {SENTINEL_1}",
            "-l",
            "python",
            "--baseline-commit",
            base_commit,
        ],
        capture_output=True,
        text=True,
        check=True,
    )
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
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(["git", "commit", "-m", "first"], check=True, capture_output=True)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], text=True
    ).strip()

    # Add head finding
    bar = git_tmp_path / "bar.py"
    bar.write_text(f"y = {SENTINEL_1}\n")
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(["git", "commit", "-m", "second"], check=True, capture_output=True)

    # Non-baseline scan should report 2 findings
    output = subprocess.run(
        [sys.executable, "-m", "semgrep", "-e", f"$X = {SENTINEL_1}", "-l", "python"],
        capture_output=True,
        text=True,
        check=True,
    )
    snapshot.assert_match(output.stdout, "output.txt")
    assert output.stdout != ""
    snapshot.assert_match(
        output.stderr.replace(base_commit, "baseline-commit"), "error.txt"
    )

    # Baseline scan should report 1 finding but hide 1
    baseline_output = subprocess.run(
        [
            sys.executable,
            "-m",
            "semgrep",
            "-e",
            f"$X = {SENTINEL_1}",
            "-l",
            "python",
            "--baseline-commit",
            base_commit,
        ],
        capture_output=True,
        text=True,
        check=True,
    )
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

    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(["git", "commit", "-m", "first"], check=True, capture_output=True)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], text=True
    ).strip()

    # Add and commit noop change
    foo.write_text(foo.read_text() + "z = 1\n")  # Note write_text overwrites
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(["git", "commit", "-m", "second"], check=True, capture_output=True)

    # Non-baseline scan should report findings
    output = subprocess.run(
        [sys.executable, "-m", "semgrep", "-e", f"$X = {SENTINEL_1}", "-l", "python"],
        capture_output=True,
        text=True,
        check=True,
    )
    snapshot.assert_match(output.stdout, "output.txt")
    assert (
        output.stdout != ""
    )  # If you fail this assertion means above snapshot was incorreclty changed
    snapshot.assert_match(output.stderr, "error.txt")

    # Baseline scan should report 0 findings
    baseline_output = subprocess.run(
        [
            sys.executable,
            "-m",
            "semgrep",
            "-e",
            f"$X = {SENTINEL_1}",
            "-l",
            "python",
            "--baseline-commit",
            base_commit,
        ],
        capture_output=True,
        text=True,
        check=True,
    )
    assert baseline_output.stdout == ""
    snapshot.assert_match(
        baseline_output.stderr.replace(base_commit, "baseline-commit"),
        "baseline_error.txt",
    )


def test_no_intersection():
    pass


def test_multiple_on_same_line():
    pass


def test_run_in_subdirectory():
    pass


def test_unstaged_changes(git_tmp_path, snapshot):
    foo = git_tmp_path / "foo"
    foo.mkdir()
    foo_a = foo / "a.py"
    foo_a.touch()
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(["git", "commit", "-m", "first"], check=True, capture_output=True)
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], text=True
    ).strip()

    foo_a.write_text("y = 55555555\n")
    output = subprocess.run(
        [
            sys.executable,
            "-m",
            "semgrep",
            "-e",
            "$X = 55555555",
            "-l",
            "python",
            "--baseline-commit",
            base_commit,
        ],
        capture_output=True,
        text=True,
    )
    assert output.returncode != 0
    snapshot.assert_match(output.stderr, "error.txt")


def test_baseline_has_head_untracked():
    pass


def test_not_git_directory(monkeypatch, tmp_path, snapshot):
    # Should abort baseline scan if not a git directory
    monkeypatch.chdir(tmp_path)
    foo = tmp_path / "foo"
    foo.mkdir()
    foo_a = foo / "a.py"
    foo_a.write_text("y = 55555555\n")

    output = subprocess.run(
        [
            sys.executable,
            "-m",
            "semgrep",
            "-e",
            "$X = 55555555",
            "-l",
            "python",
            "--baseline-commit",
            "12345",
        ],
        capture_output=True,
        text=True,
    )
    assert output.returncode != 0
    snapshot.assert_match(output.stderr, "error.txt")


def test_commit_doesnt_exist(git_tmp_path, snapshot):
    # Should abourt baseline scan if baseline is not valid commit
    foo = git_tmp_path / "foo"
    foo.mkdir()
    foo_a = foo / "a.py"
    foo_a.touch()
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    subprocess.run(["git", "commit", "-m", "first"], check=True, capture_output=True)

    output = subprocess.run(
        [
            sys.executable,
            "-m",
            "semgrep",
            "-e",
            "$X = 123",
            "-l",
            "python",
            "--baseline-commit",
            "12345",
        ],
        capture_output=True,
        text=True,
    )
    assert output.returncode != 0
    snapshot.assert_match(output.stderr, "error.txt")


@pytest.fixture
def git_tmp_path(monkeypatch, tmp_path):
    monkeypatch.chdir(tmp_path)
    # Initialize State
    subprocess.run(["git", "init"], check=True, capture_output=True)
    subprocess.run(
        ["git", "config", "user.email", "baselinetest@r2c.dev"],
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "config", "user.name", "Baseline Test"], check=True, capture_output=True
    )
    subprocess.run(["git", "checkout", "-B", "main"], check=True, capture_output=True)
    yield tmp_path
