import subprocess
from pathlib import Path
from typing import Set

from semgrep.target_manager import TargetManager


def test_filter_include():
    all_files = [
        "foo.py",
        "foo.go",
        "foo.java",
        "foo/bar.py",
        "foo/bar.go",
        "bar/foo/baz/bar.go",
        "foo/bar.java",
        "bar/baz",
        "baz.py",
        "baz.go",
        "baz.java",
        "bar/foo/foo.py",
        "foo",
        "bar/baz/foo/a.py",
        "bar/baz/foo/b.py",
        "bar/baz/foo/c.py",
        "bar/baz/qux/foo/a.py",
        "/foo/bar/baz/a.py",
    ]
    all_files = set({Path(elem) for elem in all_files})

    # All .py files
    assert len(TargetManager.filter_includes(all_files, ["*.py"])) == 9

    # All files in a foo directory ancestor
    assert len(TargetManager.filter_includes(all_files, ["foo"])) == 11

    # All files with an ancestor named bar/baz
    assert len(TargetManager.filter_includes(all_files, ["bar/baz"])) == 6

    # All go files
    assert len(TargetManager.filter_includes(all_files, ["*.go"])) == 4

    # All go and java files
    assert len(TargetManager.filter_includes(all_files, ["*.go", "*.java"])) == 7

    # All go files with a direct ancestor named foo
    assert len(TargetManager.filter_includes(all_files, ["foo/*.go"])) == 1


def test_filter_exclude():
    all_files = [
        "foo.py",
        "foo.go",
        "foo.java",
        "foo/bar.py",
        "foo/bar.go",
        "bar/foo/baz/bar.go",
        "foo/bar.java",
        "bar/baz",
        "baz.py",
        "baz.go",
        "baz.java",
        "bar/foo/foo.py",
        "foo",
        "bar/baz/foo/a.py",
        "bar/baz/foo/b.py",
        "bar/baz/foo/c.py",
        "bar/baz/qux/foo/a.py",
        "/foo/bar/baz/a.py",
    ]
    all_files = set({Path(elem) for elem in all_files})

    # Filter out .py files
    assert len(TargetManager.filter_excludes(all_files, ["*.py"])) == 9

    # Filter out files in a foo directory ancestor
    assert len(TargetManager.filter_excludes(all_files, ["foo"])) == 7

    # Filter out files with an ancestor named bar/baz
    assert len(TargetManager.filter_excludes(all_files, ["bar/baz"])) == 12

    # Filter out go files
    assert len(TargetManager.filter_excludes(all_files, ["*.go"])) == 14

    # Filter out go and java files
    assert len(TargetManager.filter_excludes(all_files, ["*.go", "*.java"])) == 11

    # Filter out go files with a direct ancestor named foo
    assert len(TargetManager.filter_excludes(all_files, ["foo/*.go"])) == 17


def test_expand_targets_git(tmp_path, monkeypatch):
    """
        Test TargetManager with visible_to_git_only flag on in a git repository
        with nested .gitignores
    """
    foo = tmp_path / "foo"
    foo.mkdir()
    foo_a_go = foo / "a.go"
    foo_a_go.touch()
    (foo / "b.go").touch()
    (foo / "py").touch()
    foo_a = foo / "a.py"
    foo_a.touch()
    foo_b = foo / "b.py"
    foo_b.touch()

    bar = tmp_path / "bar"
    bar.mkdir()
    bar_a = bar / "a.py"
    bar_a.touch()
    bar_b = bar / "b.py"
    bar_b.touch()

    foo_bar = foo / "bar"
    foo_bar.mkdir()
    foo_bar_a = foo_bar / "a.py"
    foo_bar_a.touch()
    foo_bar_b = foo_bar / "b.py"
    foo_bar_b.touch()

    monkeypatch.chdir(tmp_path)
    subprocess.run(["git", "init"])
    subprocess.run(["git", "add", foo_a])
    subprocess.run(["git", "add", foo_bar_a])
    subprocess.run(["git", "add", foo_bar_b])
    subprocess.run(["git", "add", foo_a_go])
    subprocess.run(["git", "commit", "-m", "first"])

    # Check that all files are visible without a .gitignore
    in_foo_bar = {foo_bar_a, foo_bar_b}
    in_foo = {foo_a, foo_b}.union(in_foo_bar)
    in_bar = {bar_a, bar_b}
    in_all = in_foo.union(in_bar)

    monkeypatch.chdir(tmp_path)
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], "python", True), in_all
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], "python", True), in_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo")], "python", True), in_foo
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo").resolve()], "python", True), in_foo
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo/bar")], "python", True), in_foo_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo/bar").resolve()], "python", True),
        in_foo_bar,
    )
    monkeypatch.chdir(foo)
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], "python", True), in_foo
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("./foo")], "python", True), set()
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], "python", True), in_foo_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], "python", True), in_foo_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("..")], "python", True), in_all
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("../bar")], "python", True), in_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("../foo/bar")], "python", True), in_foo_bar
    )

    # Add bar/, foo/bar/a.py, foo/b.py to gitignores
    monkeypatch.chdir(tmp_path)
    (tmp_path / ".gitignore").write_text("bar/\nfoo/bar/a.py")
    (tmp_path / "foo" / ".gitignore").write_text("b.py")

    # Reflect what should now be visible given gitignores
    in_foo_bar = {
        foo_bar_a,
        foo_bar_b,
    }  # foo/bar/a.py is gitignored but is already tracked
    in_foo = {foo_a}.union(in_foo_bar)  # foo/b.py is gitignored with a nested gitignore
    in_bar = set()  # bar/ is gitignored
    in_all = in_foo.union(in_bar)

    monkeypatch.chdir(tmp_path)
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], "python", True), in_all
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], "python", True), in_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo")], "python", True), in_foo
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo").resolve()], "python", True), in_foo
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo/bar")], "python", True), in_foo_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo/bar").resolve()], "python", True),
        in_foo_bar,
    )
    monkeypatch.chdir(foo)
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], "python", True), in_foo
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("./foo")], "python", True), set()
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], "python", True), in_foo_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], "python", True), in_foo_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("..")], "python", True), in_all
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("../bar")], "python", True), in_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("../foo/bar")], "python", True), in_foo_bar
    )


def cmp_path_sets(a: Set[Path], b: Set[Path]) -> bool:
    """
        Check that two sets of path contain the same paths
    """
    a_abs = {elem.resolve() for elem in a}
    b_abs = {elem.resolve() for elem in b}
    return a_abs == b_abs


def test_expand_targets_not_git(tmp_path, monkeypatch):
    """
        Check that directory expansion works with relative paths, absolute paths, paths with ..
    """
    foo = tmp_path / "foo"
    foo.mkdir()
    (foo / "a.go").touch()
    (foo / "b.go").touch()
    (foo / "py").touch()
    foo_a = foo / "a.py"
    foo_a.touch()
    foo_b = foo / "b.py"
    foo_b.touch()

    bar = tmp_path / "bar"
    bar.mkdir()
    bar_a = bar / "a.py"
    bar_a.touch()
    bar_b = bar / "b.py"
    bar_b.touch()

    foo_bar = foo / "bar"
    foo_bar.mkdir()
    foo_bar_a = foo_bar / "a.py"
    foo_bar_a.touch()
    foo_bar_b = foo_bar / "b.py"
    foo_bar_b.touch()

    in_foo_bar = {foo_bar_a, foo_bar_b}
    in_foo = {foo_a, foo_b}.union(in_foo_bar)
    in_bar = {bar_a, bar_b}
    in_all = in_foo.union(in_bar)

    monkeypatch.chdir(tmp_path)
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], "python", False), in_all
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], "python", False), in_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo")], "python", False), in_foo
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo").resolve()], "python", False), in_foo
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo/bar")], "python", False), in_foo_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo/bar").resolve()], "python", False),
        in_foo_bar,
    )

    monkeypatch.chdir(foo)
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], "python", False), in_foo
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("./foo")], "python", False), set()
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], "python", False), in_foo_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], "python", False), in_foo_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("..")], "python", False), in_all
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("../bar")], "python", False), in_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("../foo/bar")], "python", False), in_foo_bar
    )
