import subprocess
from pathlib import Path
from tempfile import NamedTemporaryFile
from typing import Collection

from semgrep.constants import OutputFormat
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.semgrep_types import Language
from semgrep.semgrep_types import Language_util
from semgrep.target_manager import TargetManager


def test_filter_include():
    all_file_names = [
        "/foo/bar/baz/a.py",
        "bar/baz",
        "bar/baz/foo/a.py",
        "bar/baz/foo/b.py",
        "bar/baz/foo/c.py",
        "bar/baz/qux/foo/a.py",
        "bar/foo/baz/bar.go",
        "bar/foo/foo.py",
        "baz.go",
        "baz.java",
        "baz.py",
        "baz/foo",
        "foo",
        "foo.go",
        "foo.java",
        "foo.py",
        "foo/bar.go",
        "foo/bar.java",
        "foo/bar.py",
    ]
    all_files = frozenset({Path(elem) for elem in all_file_names})

    # All .py files
    assert TargetManager.filter_includes(all_files, ["*.py"]) == {
        Path(p)
        for p in [
            "/foo/bar/baz/a.py",
            "bar/baz/foo/a.py",
            "bar/baz/foo/b.py",
            "bar/baz/foo/c.py",
            "bar/baz/qux/foo/a.py",
            "bar/foo/foo.py",
            "baz.py",
            "foo.py",
            "foo/bar.py",
        ]
    }

    # All go files
    assert TargetManager.filter_includes(all_files, ["*.go"]) == {
        Path(p)
        for p in [
            "bar/foo/baz/bar.go",
            "baz.go",
            "foo.go",
            "foo/bar.go",
        ]
    }

    # All go and java files
    assert TargetManager.filter_includes(all_files, ["*.go", "*.java"]) == {
        Path(p)
        for p in [
            "bar/foo/baz/bar.go",
            "baz.go",
            "baz.java",
            "foo.go",
            "foo.java",
            "foo/bar.go",
            "foo/bar.java",
        ]
    }

    # All files named foo or in a foo directory ancestor
    assert TargetManager.filter_includes(all_files, ["foo"]) == {
        Path(p)
        for p in [
            "/foo/bar/baz/a.py",
            "bar/baz/foo/a.py",
            "bar/baz/foo/b.py",
            "bar/baz/foo/c.py",
            "bar/baz/qux/foo/a.py",
            "bar/foo/baz/bar.go",
            "bar/foo/foo.py",
            "baz/foo",
            "foo",
            "foo/bar.go",
            "foo/bar.java",
            "foo/bar.py",
        ]
    }

    # All files with an ancestor named bar/baz
    assert TargetManager.filter_includes(all_files, ["bar/baz"]) == {
        Path(p)
        for p in [
            "/foo/bar/baz/a.py",
            "bar/baz",
            "bar/baz/foo/a.py",
            "bar/baz/foo/b.py",
            "bar/baz/foo/c.py",
            "bar/baz/qux/foo/a.py",
        ]
    }

    # All go files with a direct ancestor named foo
    assert TargetManager.filter_includes(all_files, ["foo/*.go"]) == {
        Path(p)
        for p in [
            "foo/bar.go",
        ]
    }

    # All go files with a ancestor named foo
    assert TargetManager.filter_includes(all_files, ["foo/**/*.go"]) == {
        Path(p)
        for p in [
            "bar/foo/baz/bar.go",
            "foo/bar.go",
        ]
    }

    # All py files with three-characters name
    assert TargetManager.filter_includes(all_files, ["???.py"]) == {
        Path(p)
        for p in [
            "bar/foo/foo.py",
            "baz.py",
            "foo.py",
            "foo/bar.py",
        ]
    }

    # Test some different variantions of the pattern yield the same result.
    assert TargetManager.filter_includes(
        all_files, ["baz/qux"]
    ) == TargetManager.filter_includes(all_files, ["/baz/qux"])
    assert TargetManager.filter_includes(
        all_files, ["baz/qux"]
    ) == TargetManager.filter_includes(all_files, ["baz/qux/"])
    assert TargetManager.filter_includes(
        all_files, ["baz/qux"]
    ) == TargetManager.filter_includes(all_files, ["/baz/qux/"])
    assert TargetManager.filter_includes(
        all_files, ["baz/qux"]
    ) == TargetManager.filter_includes(all_files, ["**/baz/qux"])
    assert TargetManager.filter_includes(
        all_files, ["baz/qux"]
    ) == TargetManager.filter_includes(all_files, ["baz/qux/**"])
    assert TargetManager.filter_includes(
        all_files, ["baz/qux"]
    ) == TargetManager.filter_includes(all_files, ["**/baz/qux/**"])


def test_filter_exclude():
    all_file_names = [
        "/foo/bar/baz/a.py",
        "bar/baz",
        "bar/baz/foo/a.py",
        "bar/baz/foo/b.py",
        "bar/baz/foo/c.py",
        "bar/baz/qux/foo/a.py",
        "bar/foo/baz/bar.go",
        "bar/foo/foo.py",
        "baz.go",
        "baz.java",
        "baz.py",
        "baz/foo",
        "foo",
        "foo.go",
        "foo.java",
        "foo.py",
        "foo/bar.go",
        "foo/bar.java",
        "foo/bar.py",
    ]
    all_files = frozenset({Path(elem) for elem in all_file_names})

    # Filter out .py files
    assert TargetManager.filter_excludes(all_files, ["*.py"]) == {
        Path(p)
        for p in [
            "bar/baz",
            "bar/foo/baz/bar.go",
            "baz.go",
            "baz.java",
            "baz/foo",
            "foo",
            "foo.go",
            "foo.java",
            "foo/bar.go",
            "foo/bar.java",
        ]
    }

    # Filter out go files
    assert TargetManager.filter_excludes(all_files, ["*.go"]) == {
        Path(p)
        for p in [
            "/foo/bar/baz/a.py",
            "bar/baz",
            "bar/baz/foo/a.py",
            "bar/baz/foo/b.py",
            "bar/baz/foo/c.py",
            "bar/baz/qux/foo/a.py",
            "bar/foo/foo.py",
            "baz.java",
            "baz.py",
            "baz/foo",
            "foo",
            "foo.java",
            "foo.py",
            "foo/bar.java",
            "foo/bar.py",
        ]
    }

    # Filter out go and java files
    assert TargetManager.filter_excludes(all_files, ["*.go", "*.java"]) == {
        Path(p)
        for p in [
            "/foo/bar/baz/a.py",
            "bar/baz",
            "bar/baz/foo/a.py",
            "bar/baz/foo/b.py",
            "bar/baz/foo/c.py",
            "bar/baz/qux/foo/a.py",
            "bar/foo/foo.py",
            "baz.py",
            "baz/foo",
            "foo",
            "foo.py",
            "foo/bar.py",
        ]
    }

    # Filter out files named foo or in a foo directory ancestor
    assert TargetManager.filter_excludes(all_files, ["foo"]) == {
        Path(p)
        for p in [
            "bar/baz",
            "baz.go",
            "baz.java",
            "baz.py",
            "foo.go",
            "foo.java",
            "foo.py",
        ]
    }

    # Filter out files with an ancestor named bar/baz
    assert TargetManager.filter_excludes(all_files, ["bar/baz"]) == {
        Path(p)
        for p in [
            "bar/foo/baz/bar.go",
            "bar/foo/foo.py",
            "baz.go",
            "baz.java",
            "baz.py",
            "baz/foo",
            "foo",
            "foo.go",
            "foo.java",
            "foo.py",
            "foo/bar.go",
            "foo/bar.java",
            "foo/bar.py",
        ]
    }

    # Filter out go files with a direct ancestor named foo
    assert TargetManager.filter_excludes(all_files, ["foo/*.go"]) == {
        Path(p)
        for p in [
            "/foo/bar/baz/a.py",
            "bar/baz",
            "bar/baz/foo/a.py",
            "bar/baz/foo/b.py",
            "bar/baz/foo/c.py",
            "bar/baz/qux/foo/a.py",
            "bar/foo/baz/bar.go",
            "bar/foo/foo.py",
            "baz.go",
            "baz.java",
            "baz.py",
            "baz/foo",
            "foo",
            "foo.go",
            "foo.java",
            "foo.py",
            "foo/bar.java",
            "foo/bar.py",
        ]
    }

    # Filter out go files with a ancestor named foo
    assert TargetManager.filter_excludes(all_files, ["foo/**/*.go"]) == {
        Path(p)
        for p in [
            "/foo/bar/baz/a.py",
            "bar/baz",
            "bar/baz/foo/a.py",
            "bar/baz/foo/b.py",
            "bar/baz/foo/c.py",
            "bar/baz/qux/foo/a.py",
            "bar/foo/foo.py",
            "baz.go",
            "baz.java",
            "baz.py",
            "baz/foo",
            "foo",
            "foo.go",
            "foo.java",
            "foo.py",
            "foo/bar.java",
            "foo/bar.py",
        ]
    }

    # Filter out py files with three-characters name
    assert TargetManager.filter_excludes(all_files, ["???.py"]) == {
        Path(p)
        for p in [
            "/foo/bar/baz/a.py",
            "bar/baz",
            "bar/baz/foo/a.py",
            "bar/baz/foo/b.py",
            "bar/baz/foo/c.py",
            "bar/baz/qux/foo/a.py",
            "bar/foo/baz/bar.go",
            "baz.go",
            "baz.java",
            "baz/foo",
            "foo",
            "foo.go",
            "foo.java",
            "foo/bar.go",
            "foo/bar.java",
        ]
    }


def test_filter_by_size():
    with NamedTemporaryFile() as fp:
        fp.write(b"0123456789")
        fp.flush()
        path = Path(fp.name)
        targets = frozenset({path})

        # no max size
        assert len(TargetManager.filter_by_size(targets, 0)) == 1

        # file is under max size
        assert len(TargetManager.filter_by_size(targets, 20)) == 1

        # file is over max size
        assert len(TargetManager.filter_by_size(targets, 5)) == 0


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

    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path(".")], Language_util.resolve("python"), True
        ),
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

    python_language = Language_util.resolve("python")

    monkeypatch.chdir(tmp_path)
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], python_language, True), in_all
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, True), in_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo")], python_language, True), in_foo
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo").resolve()], python_language, True),
        in_foo,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo/bar")], python_language, True),
        in_foo_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("foo/bar").resolve()], python_language, True
        ),
        in_foo_bar,
    )
    monkeypatch.chdir(foo)
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], python_language, True), in_foo
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("./foo")], python_language, True), set()
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, True), in_foo_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, True), in_foo_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("..")], python_language, True), in_all
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("../bar")], python_language, True), in_bar
    )
    assert cmp_path_sets(
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
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], python_language, True), in_all
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, True), in_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo")], python_language, True), in_foo
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo").resolve()], python_language, True),
        in_foo,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo/bar")], python_language, True),
        in_foo_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("foo/bar").resolve()], python_language, True
        ),
        in_foo_bar,
    )
    monkeypatch.chdir(foo)
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], python_language, True), in_foo
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("./foo")], python_language, True), set()
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, True), in_foo_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, True), in_foo_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("..")], python_language, True), in_all
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("../bar")], python_language, True), in_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("../foo/bar")], python_language, True),
        in_foo_bar,
    )


def cmp_path_sets(a: Collection[Path], b: Collection[Path]) -> bool:
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

    python_language = Language("python")

    monkeypatch.chdir(tmp_path)
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], python_language, False), in_all
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, False), in_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo")], python_language, False), in_foo
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo").resolve()], python_language, False),
        in_foo,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo/bar")], python_language, False),
        in_foo_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("foo/bar").resolve()], python_language, False
        ),
        in_foo_bar,
    )

    monkeypatch.chdir(foo)
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], python_language, False), in_foo
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("./foo")], python_language, False), set()
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, False), in_foo_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, False), in_foo_bar
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("..")], python_language, False), in_all
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("../bar")], python_language, False), in_bar
    )
    assert cmp_path_sets(
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

    assert cmp_path_sets(
        TargetManager.expand_targets([foo], python_language, False),
        {foo / "a.py"},
    )

    assert cmp_path_sets(
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
    output_settings = OutputSettings(
        output_format=OutputFormat.TEXT,
        output_destination=None,
        error_on_findings=False,
        verbose_errors=False,
        strict=False,
        json_stats=False,
        output_time=False,
        output_per_finding_max_lines_limit=None,
        output_per_line_max_chars_limit=None,
    )
    defaulthandler = OutputHandler(output_settings)
    assert frozenset() == TargetManager(
        [], [], 0, [foo], True, defaulthandler, False
    ).get_files(language, [], [])


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
    output_settings = OutputSettings(
        output_format=OutputFormat.TEXT,
        output_destination=None,
        error_on_findings=False,
        verbose_errors=False,
        strict=False,
        json_stats=False,
        output_time=False,
        output_per_finding_max_lines_limit=None,
        output_per_line_max_chars_limit=None,
    )
    defaulthandler = OutputHandler(output_settings)

    python_language = Language("python")

    assert foo_a in TargetManager(
        [], [], 0, ["foo/a.py"], False, defaulthandler, False
    ).get_files(python_language, [], [])
    assert foo_a in TargetManager(
        [], [], 0, ["foo/a.py"], False, defaulthandler, True
    ).get_files(python_language, [], [])

    # Should include explicitly passed python file even if is in excludes
    assert foo_a not in TargetManager(
        [], ["foo/a.py"], 0, ["."], False, defaulthandler, False
    ).get_files(python_language, [], [])
    assert foo_a in TargetManager(
        [], ["foo/a.py"], 0, [".", "foo/a.py"], False, defaulthandler, False
    ).get_files(python_language, [], [])

    # Should ignore expliclty passed .go file when requesting python
    assert (
        TargetManager([], [], 0, ["foo/a.go"], False, defaulthandler, False).get_files(
            python_language, [], []
        )
        == frozenset()
    )

    # Should include explicitly passed file with unknown extension if skip_unknown_extensions=False
    assert cmp_path_sets(
        TargetManager([], [], 0, ["foo/noext"], False, defaulthandler, False).get_files(
            python_language, [], []
        ),
        {foo_noext},
    )

    # Should not include explicitly passed file with unknown extension if skip_unknown_extensions=True
    assert cmp_path_sets(
        TargetManager([], [], 0, ["foo/noext"], False, defaulthandler, True).get_files(
            python_language, [], []
        ),
        set(),
    )

    # Should include explicitly passed file with correct extension even if skip_unknown_extensions=True
    assert cmp_path_sets(
        TargetManager(
            [], [], 0, ["foo/noext", "foo/a.py"], False, defaulthandler, True
        ).get_files(python_language, [], []),
        {foo_a},
    )
