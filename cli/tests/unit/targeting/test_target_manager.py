import os
import subprocess
from functools import partial
from pathlib import Path
from types import SimpleNamespace
from typing import Collection
from typing import List

import pytest

from semgrep.error import FilesNotFoundError
from semgrep.ignores import FileIgnore
from semgrep.semgrep_types import Language
from semgrep.target_manager import Target
from semgrep.target_manager import TargetManager


@pytest.mark.quick
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
    TargetManager(["foo/a.py"])

    with pytest.raises(FilesNotFoundError) as e:
        TargetManager(["foo/a.py", "foo/doesntexist.py"])
    assert e.value.paths == (Path("foo/doesntexist.py"),)


@pytest.mark.quick
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

    assert_path_sets_equal(Target(".", True).files(), {bar})


@pytest.mark.quick
def assert_path_sets_equal(a: Collection[Path], b: Collection[Path]):
    """
    Assert that two sets of path contain the same paths
    """
    for elem in (*a, *b):
        assert (
            not elem.is_symlink()
        ), f"{elem} is a symlink so we cannot determine if it's the same as its counterpart in the other set"
    a_abs = {elem.resolve() for elem in a}
    b_abs = {elem.resolve() for elem in b}
    assert a_abs == b_abs


@pytest.mark.quick
@pytest.fixture(
    scope="session", params=["no-repo", "git-repo", "git-repo-with-ignores"]
)
def paths(request, tmp_path_factory):
    git_mode = request.param
    tmp_path = tmp_path_factory.mktemp("repo")
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

    if git_mode != "no-repo":
        subprocess.run(["git", "init"], cwd=tmp_path)
        subprocess.run(["git", "add", foo_a], cwd=tmp_path)
        subprocess.run(["git", "add", foo_bar_a], cwd=tmp_path)
        subprocess.run(["git", "add", foo_bar_b], cwd=tmp_path)
        subprocess.run(["git", "add", foo_a_go], cwd=tmp_path)
        subprocess.run(["git", "commit", "-m", "first"], cwd=tmp_path)

    if git_mode == "git-repo-with-ignores":
        (tmp_path / ".gitignore").write_text("bar/\nfoo/bar/a.py")
        (tmp_path / "foo" / ".gitignore").write_text("b.py")

    class Paths(SimpleNamespace):
        root = tmp_path
        foo_bar = {foo_bar_a, foo_bar_b}
        foo = {foo_a, foo_b}.union(foo_bar)
        bar = {bar_a, bar_b}

        if git_mode == "git-repo-with-ignores":
            # Reflect what should now be visible given gitignores
            # foo_bar is unchanged: foo/bar/a.py is gitignored but is already tracked
            foo = {foo_a, *foo_bar}  # foo/b.py is gitignored with a nested gitignore
            bar = set()  # bar/ is gitignored

        all = foo | bar

        TargetManager = (
            TargetManager
            if git_mode is None
            else partial(partial(TargetManager, respect_git_ignore=True))
        )

    yield Paths


LANG_PY = Language("python")
LANG_GENERIC = Language("generic")
LANG_REGEX = Language("regex")


@pytest.mark.quick
@pytest.mark.parametrize(
    "workdir, targets, expected",
    [
        ("/", ["."], "all"),
        ("/", ["foo", "bar"], "all"),
        ("/", ["bar"], "bar"),
        ("/", ["foo"], "foo"),
        ("/", ["foo/bar"], "foo_bar"),
        ("/foo", ["."], "foo"),
        ("/foo", ["./foo"], None),
        ("/foo", ["bar"], "foo_bar"),
        ("/foo", [".."], "all"),
        ("/foo", ["../bar"], "bar"),
        ("/foo", ["../foo", "../bar"], "all"),
        ("/foo", ["../foo/bar"], "foo_bar"),
        ("/foo/bar", ["../.."], "all"),
    ],
    ids=str,
)
@pytest.mark.parametrize("referencing", ["relative", "absolute"])
def test_get_files_for_language(
    paths, monkeypatch, workdir, targets, expected, referencing
):
    monkeypatch.chdir(paths.root / workdir.strip("/"))

    if referencing == "absolute":
        targets = [str(Path(target).resolve()) for target in targets]

    if expected is None:
        with pytest.raises(FilesNotFoundError):
            target_manager = paths.TargetManager(targets)
        return
    else:
        target_manager = paths.TargetManager(targets)

    actual = target_manager.get_files_for_language(LANG_PY).kept

    assert_path_sets_equal(actual, getattr(paths, expected))


@pytest.mark.quick
def test_skip_symlink(tmp_path, monkeypatch):
    foo = tmp_path / "foo"
    foo.mkdir()
    (foo / "a.py").touch()
    (foo / "link.py").symlink_to(foo / "a.py")

    monkeypatch.chdir(tmp_path)

    PY = Language("python")

    assert_path_sets_equal(
        TargetManager([str(foo)]).get_files_for_language(PY).kept,
        {foo / "a.py"},
    )

    with pytest.raises(FilesNotFoundError):
        TargetManager([str(foo / "link.py")]).get_files_for_language(PY)


@pytest.mark.quick
def test_ignore_git_dir(tmp_path, monkeypatch):
    """
    Ignores all files in .git directory when scanning generic
    """
    foo = tmp_path / ".git"
    foo.mkdir()
    (foo / "bar").touch()

    monkeypatch.chdir(tmp_path)
    language = Language("generic")
    assert frozenset() == TargetManager([foo]).get_files_for_rule(
        language, [], [], "dummy_rule_id"
    )


@pytest.mark.quick
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
        ["foo/a.py"], allow_unknown_extensions=True
    ).get_files_for_rule(python_language, [], [], "dummy_rule_id")
    assert foo_a in TargetManager(["foo/a.py"]).get_files_for_rule(
        python_language, [], [], "dummy_rule_id"
    )

    # Should include explicitly passed python file even if is in excludes
    assert foo_a not in TargetManager(["."], [], ["foo/a.py"]).get_files_for_rule(
        python_language, [], [], "dummy_rule_id"
    )
    assert foo_a in TargetManager(
        [".", "foo/a.py"], [], ["foo/a.py"]
    ).get_files_for_rule(python_language, [], [], "dummy_rule_id")

    # Should ignore expliclty passed .go file when requesting python
    assert (
        TargetManager(["foo/a.go"]).get_files_for_rule(
            python_language, [], [], "dummy_rule_id"
        )
        == frozenset()
    )

    # Should include explicitly passed file with unknown extension if allow_unknown_extensions=True
    assert_path_sets_equal(
        TargetManager(["foo/noext"], allow_unknown_extensions=True).get_files_for_rule(
            python_language, [], [], "dummy_rule_id"
        ),
        {foo_noext},
    )

    # Should not include explicitly passed file with unknown extension by default
    assert_path_sets_equal(
        TargetManager(["foo/noext"]).get_files_for_rule(
            python_language, [], [], "dummy_rule_id"
        ),
        set(),
    )

    # Should include explicitly passed file with correct extension even if skip_unknown_extensions=True
    assert_path_sets_equal(
        TargetManager(["foo/noext", "foo/a.py"]).get_files_for_rule(
            python_language, [], [], "dummy_rule_id"
        ),
        {foo_a},
    )

    # Should respect includes/excludes passed to get_files even if target explicitly passed
    assert_path_sets_equal(
        TargetManager(["foo/a.py", "foo/b.py"]).get_files_for_rule(
            python_language, ["a.py"], [], "dummy_rule_id"
        ),
        {foo_a},
    )


@pytest.mark.quick
def test_ignores(tmp_path, monkeypatch):
    def ignore(ignore_pats):
        return TargetManager(
            [tmp_path],
            file_ignore=FileIgnore.from_unprocessed_patterns(tmp_path, ignore_pats),
        ).get_files_for_rule(Language("python"), [], [], "dummy_rule_id")

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


@pytest.mark.quick
def test_unsupported_lang_paths(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)

    targets: List[str] = []

    # we will "scan" only for python---others will be unsupported
    paths = {
        ".": ["a.py", "b.py", "c.rb", "d.rb", "e.erb"],
        "dir": ["f.erb", "g.rkt", "h.rkt", "i.rkt"],
    }

    expected_unsupported = set()

    for dir_name in paths:
        dir = tmp_path
        if not dir_name == ".":
            dir = tmp_path / dir_name
            dir.mkdir()
        for file_name in paths[dir_name]:
            path = dir / file_name
            path.touch()
            targets.append(str(path))
            if os.path.splitext(path)[1] != ".py":
                expected_unsupported.add(path)

    target_manager = TargetManager(targets)

    target_manager.get_files_for_language(LANG_PY)
    target_manager.get_files_for_language(LANG_GENERIC)
    target_manager.get_files_for_language(LANG_REGEX)

    assert_path_sets_equal(
        target_manager.ignore_log.unsupported_lang_paths, expected_unsupported
    )


@pytest.mark.quick
def test_unsupported_lang_paths_2(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)

    targets: List[str] = []

    # we will "scan" only for generic and regex
    paths = {
        ".": ["a.rb", "b.erb"],
        "dir": ["c.erb", "d.rkt"],
    }

    expected_unsupported = set()

    for dir_name in paths:
        dir = tmp_path
        if not dir_name == ".":
            dir = tmp_path / dir_name
            dir.mkdir()
        for file_name in paths[dir_name]:
            path = dir / file_name
            path.touch()
            targets.append(str(path))
            expected_unsupported.add(path)

    target_manager = TargetManager(targets)

    target_manager.get_files_for_language(LANG_GENERIC)
    target_manager.get_files_for_language(LANG_REGEX)

    assert_path_sets_equal(
        target_manager.ignore_log.unsupported_lang_paths, expected_unsupported
    )
