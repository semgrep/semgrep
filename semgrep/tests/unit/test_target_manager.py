import subprocess
from pathlib import Path
from typing import NewType
from typing import Set

from semgrep.constants import OutputFormat
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.semgrep_types import Language
from semgrep.target_manager import TargetManager

FileExtension = NewType("FileExtension", str)

PYTHON_EXTENSIONS = [FileExtension("py"), FileExtension("pyi")]
JAVASCRIPT_EXTENSIONS = [
    FileExtension("js"),
    FileExtension("jsx"),
    FileExtension("ts"),
    FileExtension("tsx"),
]
TYPESCRIPT_EXTENSIONS = [FileExtension("ts"), FileExtension("tsx")]
JAVA_EXTENSIONS = [FileExtension("java")]
C_EXTENSIONS = [FileExtension("c")]
GO_EXTENSIONS = [FileExtension("go")]
RUBY_EXTENSIONS = [FileExtension("rb")]
PHP_EXTENSIONS = [FileExtension("php")]
ML_EXTENSIONS = [
    FileExtension("mli"),
    FileExtension("ml"),
]
JSON_EXTENSIONS = [FileExtension("json")]
ALL_EXTENSIONS = (
    PYTHON_EXTENSIONS
    + JAVASCRIPT_EXTENSIONS
    + TYPESCRIPT_EXTENSIONS
    + JAVA_EXTENSIONS
    + C_EXTENSIONS
    + GO_EXTENSIONS
    + RUBY_EXTENSIONS
    + ML_EXTENSIONS
    + JSON_EXTENSIONS
)


def test_filter_include():
    all_file_names = [
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
    all_files = set({Path(elem) for elem in all_file_names})

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
    all_file_names = [
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
    all_files = set({Path(elem) for elem in all_file_names})

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

    extensions = {
        "python_extensions": PYTHON_EXTENSIONS,
        "javascript_extensions": JAVASCRIPT_EXTENSIONS,
        "typescript_extensions": TYPESCRIPT_EXTENSIONS,
        "java_extensions": JAVA_EXTENSIONS,
        "c_extensions": C_EXTENSIONS,
        "go_extensions": GO_EXTENSIONS,
        "ruby_extensions": RUBY_EXTENSIONS,
        "php_extensions": PHP_EXTENSIONS,
        "ml_extensions": ML_EXTENSIONS,
        "json_extensions": JSON_EXTENSIONS,
        "all_extensions": ALL_EXTENSIONS,
    }

    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], Language("python"), True, extensions),
        {bar},
    )


def test_expand_targets_git(tmp_path, monkeypatch):
    """
        Test TargetManager with visible_to_git_only flag on in a git repository
        with nested .gitignores
    """
    extensions = {
        "python_extensions": PYTHON_EXTENSIONS,
        "javascript_extensions": JAVASCRIPT_EXTENSIONS,
        "typescript_extensions": TYPESCRIPT_EXTENSIONS,
        "java_extensions": JAVA_EXTENSIONS,
        "c_extensions": C_EXTENSIONS,
        "go_extensions": GO_EXTENSIONS,
        "ruby_extensions": RUBY_EXTENSIONS,
        "php_extensions": PHP_EXTENSIONS,
        "ml_extensions": ML_EXTENSIONS,
        "json_extensions": JSON_EXTENSIONS,
        "all_extensions": ALL_EXTENSIONS,
    }

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

    python_language = Language("python")

    monkeypatch.chdir(tmp_path)
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], python_language, True, extensions),
        in_all,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, True, extensions),
        in_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo")], python_language, True, extensions),
        in_foo,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("foo").resolve()], python_language, True, extensions
        ),
        in_foo,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("foo/bar")], python_language, True, extensions
        ),
        in_foo_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("foo/bar").resolve()], python_language, True, extensions
        ),
        in_foo_bar,
    )
    monkeypatch.chdir(foo)
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], python_language, True, extensions),
        in_foo,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("./foo")], python_language, True, extensions
        ),
        set(),
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, True, extensions),
        in_foo_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, True, extensions),
        in_foo_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("..")], python_language, True, extensions),
        in_all,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("../bar")], python_language, True, extensions
        ),
        in_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("../foo/bar")], python_language, True, extensions
        ),
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
        TargetManager.expand_targets([Path(".")], python_language, True, extensions),
        in_all,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, True, extensions),
        in_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo")], python_language, True, extensions),
        in_foo,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("foo").resolve()], python_language, True, extensions
        ),
        in_foo,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("foo/bar")], python_language, True, extensions
        ),
        in_foo_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("foo/bar").resolve()], python_language, True, extensions
        ),
        in_foo_bar,
    )
    monkeypatch.chdir(foo)
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], python_language, True, extensions),
        in_foo,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("./foo")], python_language, True, extensions
        ),
        set(),
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, True, extensions),
        in_foo_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, True, extensions),
        in_foo_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("..")], python_language, True, extensions),
        in_all,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("../bar")], python_language, True, extensions
        ),
        in_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("../foo/bar")], python_language, True, extensions
        ),
        in_foo_bar,
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
    extensions = {
        "python_extensions": PYTHON_EXTENSIONS,
        "javascript_extensions": JAVASCRIPT_EXTENSIONS,
        "typescript_extensions": TYPESCRIPT_EXTENSIONS,
        "java_extensions": JAVA_EXTENSIONS,
        "c_extensions": C_EXTENSIONS,
        "go_extensions": GO_EXTENSIONS,
        "ruby_extensions": RUBY_EXTENSIONS,
        "php_extensions": PHP_EXTENSIONS,
        "ml_extensions": ML_EXTENSIONS,
        "json_extensions": JSON_EXTENSIONS,
        "all_extensions": ALL_EXTENSIONS,
    }

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
        TargetManager.expand_targets([Path(".")], python_language, False, extensions),
        in_all,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, False, extensions),
        in_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("foo")], python_language, False, extensions),
        in_foo,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("foo").resolve()], python_language, False, extensions
        ),
        in_foo,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("foo/bar")], python_language, False, extensions
        ),
        in_foo_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("foo/bar").resolve()], python_language, False, extensions
        ),
        in_foo_bar,
    )

    monkeypatch.chdir(foo)
    assert cmp_path_sets(
        TargetManager.expand_targets([Path(".")], python_language, False, extensions),
        in_foo,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("./foo")], python_language, False, extensions
        ),
        set(),
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, False, extensions),
        in_foo_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("bar")], python_language, False, extensions),
        in_foo_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets([Path("..")], python_language, False, extensions),
        in_all,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("../bar")], python_language, False, extensions
        ),
        in_bar,
    )
    assert cmp_path_sets(
        TargetManager.expand_targets(
            [Path("../foo/bar")], python_language, False, extensions
        ),
        in_foo_bar,
    )


def test_explicit_path(tmp_path, monkeypatch):
    extensions = {
        "python_extensions": PYTHON_EXTENSIONS,
        "javascript_extensions": JAVASCRIPT_EXTENSIONS,
        "typescript_extensions": TYPESCRIPT_EXTENSIONS,
        "java_extensions": JAVA_EXTENSIONS,
        "c_extensions": C_EXTENSIONS,
        "go_extensions": GO_EXTENSIONS,
        "ruby_extensions": RUBY_EXTENSIONS,
        "php_extensions": PHP_EXTENSIONS,
        "ml_extensions": ML_EXTENSIONS,
        "json_extensions": JSON_EXTENSIONS,
        "all_extensions": ALL_EXTENSIONS,
    }

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
    )
    defaulthandler = OutputHandler(output_settings)

    python_language = Language("python")

    assert foo_a in TargetManager(
        [], [], ["foo/a.py"], False, defaulthandler, False, extensions
    ).get_files(python_language, [], [])
    assert foo_a in TargetManager(
        [], [], ["foo/a.py"], False, defaulthandler, True, extensions
    ).get_files(python_language, [], [])

    # Should include explicitly passed python file even if is in excludes
    assert foo_a not in TargetManager(
        [], ["foo/a.py"], ["."], False, defaulthandler, False, extensions
    ).get_files(python_language, [], [])
    assert foo_a in TargetManager(
        [], ["foo/a.py"], [".", "foo/a.py"], False, defaulthandler, False, extensions
    ).get_files(python_language, [], [])

    # Should ignore expliclty passed .go file when requesting python
    assert (
        TargetManager(
            [], [], ["foo/a.go"], False, defaulthandler, False, extensions
        ).get_files(python_language, [], [])
        == []
    )

    # Should include explicitly passed file with unknown extension if skip_unknown_extensions=False
    assert cmp_path_sets(
        set(
            TargetManager(
                [], [], ["foo/noext"], False, defaulthandler, False, extensions
            ).get_files(python_language, [], [])
        ),
        {foo_noext},
    )

    # Should not include explicitly passed file with unknown extension if skip_unknown_extensions=True
    assert cmp_path_sets(
        set(
            TargetManager(
                [], [], ["foo/noext"], False, defaulthandler, True, extensions
            ).get_files(python_language, [], [])
        ),
        set(),
    )

    # Should include explicitly passed file with correct extension even if skip_unknown_extensions=True
    assert cmp_path_sets(
        set(
            TargetManager(
                [],
                [],
                ["foo/noext", "foo/a.py"],
                False,
                defaulthandler,
                True,
                extensions,
            ).get_files(python_language, [], [])
        ),
        {foo_a},
    )
