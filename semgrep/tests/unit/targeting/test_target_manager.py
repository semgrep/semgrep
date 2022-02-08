import subprocess
from pathlib import Path
from typing import Collection

import pytest

from semgrep.error import FilesNotFoundError
from semgrep.ignores import FileIgnore
from semgrep.semgrep_types import LANGUAGE
from semgrep.semgrep_types import Language
from semgrep.target_manager import TargetManager


def test_nonexistent(tmp_path, monkeypatch):
    """
    Test that initializing TargetManager with targets that do not exist
    raises FilesNotFoundError
    """
    foo = tmp_path / "foo"
    foo.mkdir()
    foo_a = foo / "a.py"
    foo_a.touch()

    monkeypatch.chdir(tmp_path)

    # shouldnt raise an error
    TargetManager([], [], 0, ["foo/a.py"], True, False, None)

    with pytest.raises(FilesNotFoundError) as e:
        TargetManager([], [], 0, ["foo/a.py", "foo/doesntexist.py"], True, False, None)
    assert e.value.paths == (Path("foo/doesntexist.py"),)


def test_delete_git(tmp_path, monkeypatch):
    """
    Check that deleted files are not included in expanded targets
    """
    foo = tmp_path / "foo.py"
    bar = tmp_path / "bar.py"
    foo.touch()
    bar.touch()

    monkeypatch.chdir(tmp_path)
    subprocess.run(["git", "init"])
    subprocess.run(["git", "add", foo])
    subprocess.run(["git", "commit", "-m", "first commit"])

    foo.unlink()
    subprocess.run(["git", "status"])

    assert_path_sets_equal(
        TargetManager.expand_targets([Path(".")], LANGUAGE.resolve("python"), True),
        {bar},
    )


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

    python_language = LANGUAGE.resolve("python")

    monkeypatch.chdir(tmp_path)
    assert_path_sets_equal(
        TargetManager.expand_targets([Path(".")], python_language, True), in_all
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("bar")], python_language, True), in_bar
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("foo")], python_language, True), in_foo
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("foo").resolve()], python_language, True),
        in_foo,
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("foo/bar")], python_language, True),
        in_foo_bar,
    )
    assert_path_sets_equal(
        TargetManager.expand_targets(
            [Path("foo/bar").resolve()], python_language, True
        ),
        in_foo_bar,
    )
    monkeypatch.chdir(foo)
    assert_path_sets_equal(
        TargetManager.expand_targets([Path(".")], python_language, True), in_foo
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("./foo")], python_language, True), set()
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("bar")], python_language, True), in_foo_bar
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("bar")], python_language, True), in_foo_bar
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("..")], python_language, True), in_all
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("../bar")], python_language, True), in_bar
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("../foo/bar")], python_language, True),
        in_foo_bar,
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
    assert_path_sets_equal(
        TargetManager.expand_targets([Path(".")], python_language, True), in_all
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("bar")], python_language, True), in_bar
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("foo")], python_language, True), in_foo
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("foo").resolve()], python_language, True),
        in_foo,
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("foo/bar")], python_language, True),
        in_foo_bar,
    )
    assert_path_sets_equal(
        TargetManager.expand_targets(
            [Path("foo/bar").resolve()], python_language, True
        ),
        in_foo_bar,
    )
    monkeypatch.chdir(foo)
    assert_path_sets_equal(
        TargetManager.expand_targets([Path(".")], python_language, True), in_foo
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("./foo")], python_language, True), set()
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("bar")], python_language, True), in_foo_bar
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("bar")], python_language, True), in_foo_bar
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("..")], python_language, True), in_all
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("../bar")], python_language, True), in_bar
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("../foo/bar")], python_language, True),
        in_foo_bar,
    )


def assert_path_sets_equal(a: Collection[Path], b: Collection[Path]) -> bool:
    """
    Assert that two sets of path contain the same paths
    """
    a_abs = {elem.resolve() for elem in a}
    b_abs = {elem.resolve() for elem in b}
    assert a_abs == b_abs


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

    python_language = Language("python")

    monkeypatch.chdir(tmp_path)
    assert_path_sets_equal(
        TargetManager.expand_targets([Path(".")], python_language, False), in_all
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("bar")], python_language, False), in_bar
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("foo")], python_language, False), in_foo
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("foo").resolve()], python_language, False),
        in_foo,
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("foo/bar")], python_language, False),
        in_foo_bar,
    )
    assert_path_sets_equal(
        TargetManager.expand_targets(
            [Path("foo/bar").resolve()], python_language, False
        ),
        in_foo_bar,
    )

    monkeypatch.chdir(foo)
    assert_path_sets_equal(
        TargetManager.expand_targets([Path(".")], python_language, False), in_foo
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("./foo")], python_language, False), set()
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("bar")], python_language, False), in_foo_bar
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("bar")], python_language, False), in_foo_bar
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("..")], python_language, False), in_all
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("../bar")], python_language, False), in_bar
    )
    assert_path_sets_equal(
        TargetManager.expand_targets([Path("../foo/bar")], python_language, False),
        in_foo_bar,
    )


def test_skip_symlink(tmp_path, monkeypatch):
    foo = tmp_path / "foo"
    foo.mkdir()
    (foo / "a.py").touch()
    (foo / "link.py").symlink_to(foo / "a.py")

    monkeypatch.chdir(tmp_path)

    python_language = Language("python")

    assert_path_sets_equal(
        TargetManager.expand_targets([foo], python_language, False),
        {foo / "a.py"},
    )

    assert_path_sets_equal(
        TargetManager.expand_targets([foo / "link.py"], python_language, False), set()
    )


def test_ignore_git_dir(tmp_path, monkeypatch):
    """
    Ignores all files in .git directory when scanning generic
    """
    foo = tmp_path / ".git"
    foo.mkdir()
    (foo / "bar").touch()

    monkeypatch.chdir(tmp_path)
    language = Language("generic")
    assert frozenset() == TargetManager([], [], 0, [foo], True, False, None).get_files(
        language, [], [], "dummy_rule_id"
    )


def test_explicit_path(tmp_path, monkeypatch):
    foo = tmp_path / "foo"
    foo.mkdir()
    (foo / "a.go").touch()
    (foo / "b.go").touch()
    foo_noext = foo / "noext"
    foo_noext.touch()
    foo_a = foo / "a.py"
    foo_a.touch()
    foo_b = foo / "b.py"
    foo_b.touch()

    monkeypatch.chdir(tmp_path)

    # Should include explicitly passed python file
    foo_a = foo_a.relative_to(tmp_path)
    python_language = Language("python")

    assert foo_a in TargetManager(
        [], [], 0, ["foo/a.py"], False, False, None
    ).get_files(python_language, [], [], "dummy_rule_id")
    assert foo_a in TargetManager([], [], 0, ["foo/a.py"], False, True, None).get_files(
        python_language, [], [], "dummy_rule_id"
    )

    # Should include explicitly passed python file even if is in excludes
    assert foo_a not in TargetManager(
        [], ["foo/a.py"], 0, ["."], False, False, None
    ).get_files(python_language, [], [], "dummy_rule_id")
    assert foo_a in TargetManager(
        [], ["foo/a.py"], 0, [".", "foo/a.py"], False, False, None
    ).get_files(python_language, [], [], "dummy_rule_id")

    # Should ignore expliclty passed .go file when requesting python
    assert (
        TargetManager([], [], 0, ["foo/a.go"], False, False, None).get_files(
            python_language, [], [], "dummy_rule_id"
        )
        == frozenset()
    )

    # Should include explicitly passed file with unknown extension if skip_unknown_extensions=False
    assert_path_sets_equal(
        TargetManager([], [], 0, ["foo/noext"], False, False, None).get_files(
            python_language, [], [], "dummy_rule_id"
        ),
        {foo_noext},
    )

    # Should not include explicitly passed file with unknown extension if skip_unknown_extensions=True
    assert_path_sets_equal(
        TargetManager([], [], 0, ["foo/noext"], False, True, None).get_files(
            python_language, [], [], "dummy_rule_id"
        ),
        set(),
    )

    # Should include explicitly passed file with correct extension even if skip_unknown_extensions=True
    assert_path_sets_equal(
        TargetManager(
            [], [], 0, ["foo/noext", "foo/a.py"], False, True, None
        ).get_files(python_language, [], [], "dummy_rule_id"),
        {foo_a},
    )

    # Should respect includes/excludes passed to get_files even if target explicitly passed
    assert_path_sets_equal(
        TargetManager(
            [], [], 0, ["foo/a.py", "foo/b.py"], False, False, None
        ).get_files(python_language, ["a.py"], [], "dummy_rule_id"),
        {foo_a},
    )


def test_ignores(tmp_path, monkeypatch):
    def ignore(ignore_pats):
        return TargetManager(
            [], [], 0, [tmp_path], False, False, FileIgnore(tmp_path, ignore_pats)
        ).get_files(Language("python"), [], [], "dummy_rule_id")

    monkeypatch.chdir(tmp_path)
    a = tmp_path / "a.py"
    a.touch()

    dir = tmp_path / "dir"
    dir.mkdir()

    dir_a = dir / "a.py"
    dir_a.touch()

    dir_b = dir / "b.py"
    dir_b.touch()

    dir_c = dir / "c.py"
    dir_c.touch()

    dir2 = dir / "dir2"
    dir2.mkdir()

    dir3 = dir2 / "dir3"
    dir3.mkdir()
    dir3_a = dir3 / "a.py"
    dir3_a.touch()

    # Ignore nothing
    files = ignore([])
    assert a in files

    # Ignore file name
    files = ignore(["a.py"])
    assert a not in files
    assert dir3_a not in files

    # Ignore directory
    files = ignore(["dir/"])
    assert dir_a not in files
    assert dir_b not in files
    assert dir_c not in files
    assert dir3_a not in files

    # Ignore root file
    files = ignore(["/a.py"])
    assert dir3_a in files

    # Ignore root file that does not exist
    files = ignore(["/b.py"])
    assert dir_b in files

    # Ignore not nested
    files = ignore(["dir2/dir3/a.py"])
    assert dir3_a in files

    # Ignore nested dir syntax
    files = ignore(["dir3/"])
    assert dir3_a not in files

    # Ingore nested double star
    files = ignore(["**/dir2/dir3/"])
    assert dir3_a not in files
