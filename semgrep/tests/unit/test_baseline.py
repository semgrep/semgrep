import subprocess

import pytest

from semgrep.git import BaselineHandler


def test_baseline_context(monkeypatch, tmp_path):
    """
    Unit test verifies baseline_context can checkout a commit and return to
    old state
    """
    monkeypatch.chdir(tmp_path)

    # Initialize State
    subprocess.check_call(["git", "init"])
    subprocess.check_call(["git", "config", "user.email", "baselinetest@r2c.dev"])
    subprocess.check_call(["git", "config", "user.name", "Baseline Test"])
    subprocess.check_call(["git", "checkout", "-B", "main"])

    # Create foo/a.py, foo/b.py, and foo/c.py
    foo = tmp_path / "foo"
    foo.mkdir()
    foo_a = foo / "a.py"
    foo_a.touch()
    foo_b = foo / "b.py"
    foo_b.write_text("z = 7777777")
    foo_c = foo / "c.py"
    foo_c.write_text("x = 11111111\n")

    # Add and commit foo/a.py, foo/b.py, and foo/c.py
    subprocess.check_call(["git", "add", "."])
    subprocess.check_call(["git", "commit", "-m", "first"])
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Create bar/a.py and modify foo/a.py and remove foo/b.py and rename foo/c.py
    bar = tmp_path / "bar"
    bar.mkdir()
    bar_a = bar / "a.py"
    bar_a.touch()
    foo_a.write_text("y = 55555555\n")
    foo_b.unlink()
    foo_d = foo / "d.py"
    foo_c.rename(foo_d)

    # Add and commit all changes
    subprocess.check_call(["git", "add", "."])
    # baseline_handler aborts on pending changes
    with pytest.raises(Exception) as e:
        BaselineHandler(base_commit)
    assert "Found pending changes" in str(e)
    subprocess.check_call(["git", "commit", "-m", "second"])

    baseline_handler = BaselineHandler(base_commit)
    with baseline_handler.baseline_context():
        # foo/a.py should not have written text
        assert foo_a.read_text() == ""
        assert foo_b.exists()
        assert foo_c.exists()

        assert not bar_a.exists()
        assert not foo_d.exists()

    assert foo_a.read_text() == "y = 55555555\n"
    assert not foo_b.exists()
    assert not foo_c.exists()
    assert bar_a.exists()
    assert foo_d.read_text() == "x = 11111111\n"
