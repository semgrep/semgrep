import os
import subprocess
from functools import partial
from pathlib import Path
from types import SimpleNamespace
from typing import Collection
from typing import List
from typing import Optional

import pytest

from semgrep.error import InvalidScanningRootError
from semgrep.git import BaselineHandler
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.semgrep_types import Language
from semgrep.target_manager import SAST_PRODUCT
from semgrep.target_manager import SCA_PRODUCT
from semgrep.target_manager import SECRETS_PRODUCT
from semgrep.target_manager import TargetManager


def assert_path_sets_equal(
    *, actual: Collection[Path], expected: Collection[Path], msg: Optional[str] = None
):
    """
    Assert that two sets of path contain the same paths
    """
    if msg is not None:
        prefix = f"{msg}: "
    else:
        prefix = ""

    for elem in (*actual, *expected):
        assert (
            not elem.is_symlink()
        ), f"{prefix}{elem} is a symlink so we cannot determine if it's the same as its counterpart in the other set"
    actual_paths = {elem.resolve() for elem in actual}
    expected_paths = {elem.resolve() for elem in expected}
    assert (
        actual_paths == expected_paths
    ), f"{prefix}actual (left) and expected (right) sets of target paths differ"


@pytest.mark.quick
def test_nonexistent(tmp_path, monkeypatch):
    """
    Test that initializing TargetManager with targets that do not exist
    raises InvalidScanningRootError
    """
    foo = tmp_path / "foo"
    foo.mkdir()
    foo_a = foo / "a.py"
    foo_a.touch()

    monkeypatch.chdir(tmp_path)

    # shouldn't raise an error
    TargetManager(
        scanning_root_strings=frozenset([Path("foo/a.py")]),
    )

    with pytest.raises(InvalidScanningRootError) as e:
        TargetManager(
            scanning_root_strings=frozenset(
                [Path("foo/a.py"), Path("foo/doesntexist.py")]
            ),
        )
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

    target_manager = TargetManager(
        scanning_root_strings=frozenset([Path(".")]), respect_git_ignore=True
    )
    scan_root = target_manager.scanning_roots[0]
    assert_path_sets_equal(
        actual=scan_root.target_files(product=SAST_PRODUCT),
        expected={bar},
    )


@pytest.fixture(scope="session", params=["no-repo", "git-repo"])
def repo_paths(request, tmp_path_factory):
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

    # Show us the files (but not the contents of .git/ because it's too much)
    print("ls -la:")
    subprocess.run(["ls", "-la"], cwd=tmp_path)
    print("ls -lR:")
    subprocess.run(["ls", "-lR"], cwd=tmp_path)

    class Paths(SimpleNamespace):
        root = tmp_path
        foo_bar = {foo_bar_a, foo_bar_b}
        foo = {foo_a, foo_b}.union(foo_bar)
        bar = {bar_a, bar_b}
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
    "workdir, scanning_roots, expected",
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
    # avoid square brackets in test names so they can be selected with -k
    ids=lambda x: ",".join(x) if isinstance(x, list) else str(x),
)
@pytest.mark.parametrize("referencing", ["relative", "absolute"])
def test_get_files_for_language(
    repo_paths,
    monkeypatch,
    workdir,
    scanning_roots,
    expected,
    referencing,
):
    monkeypatch.chdir(repo_paths.root / workdir.strip("/"))

    if referencing == "absolute":
        scanning_roots = [
            str(Path(scanning_root).resolve()) for scanning_root in scanning_roots
        ]

    if expected is None:
        with pytest.raises(InvalidScanningRootError):
            target_manager = repo_paths.TargetManager(
                scanning_root_strings=scanning_roots,
            )
        return
    else:
        target_manager = repo_paths.TargetManager(
            scanning_root_strings=scanning_roots,
        )

    actual = target_manager.get_files_for_language(
        lang=LANG_PY, product=SAST_PRODUCT
    ).kept

    assert_path_sets_equal(actual=actual, expected=getattr(repo_paths, expected))


@pytest.mark.quick
def test_skip_symlink(tmp_path, monkeypatch):
    foo = tmp_path / "foo"
    foo.mkdir()
    (foo / "a.py").touch()
    (foo / "link.py").symlink_to(foo / "a.py")

    monkeypatch.chdir(tmp_path)

    PY = Language("python")

    # Scan the foo/ folder.
    target_manager = TargetManager(
        scanning_root_strings=frozenset([foo]),
    )

    # Should select the regular file and not the symlink.
    assert_path_sets_equal(
        actual=target_manager.get_all_files(product=SAST_PRODUCT),
        expected={foo / "a.py"},
        msg="all files",
    )

    # Select only the Python files.
    assert_path_sets_equal(
        actual=target_manager.get_files_for_language(
            lang=PY, product=SAST_PRODUCT
        ).kept,
        expected={foo / "a.py"},
        msg="Python files",
    )

    with pytest.raises(InvalidScanningRootError):
        TargetManager(
            scanning_root_strings=frozenset([foo / "link.py"]),
        ).get_files_for_language(lang=PY, product=SAST_PRODUCT)


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
    assert (
        frozenset()
        == TargetManager(
            scanning_root_strings=frozenset([foo]),
        )
        .get_files_for_rule(language, [], [], "dummy_rule_id", SAST_PRODUCT)
        .targets
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

    assert (
        foo_a
        in TargetManager(
            scanning_root_strings=frozenset([Path("foo/a.py")]),
            allow_unknown_extensions=True,
        )
        .get_files_for_rule(python_language, [], [], "dummy_rule_id", SAST_PRODUCT)
        .targets
    )
    assert (
        foo_a
        in TargetManager(
            scanning_root_strings=frozenset([Path("foo/a.py")]),
        )
        .get_files_for_rule(python_language, [], [], "dummy_rule_id", SAST_PRODUCT)
        .targets
    )

    # Should exclude discovered python file even if it is in excludes
    assert (
        foo_a
        not in TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
            includes=[],
            excludes={SAST_PRODUCT: ["foo/a.py"]},
        )
        .get_files_for_rule(python_language, [], [], "dummy_rule_id", SAST_PRODUCT)
        .targets
    )

    # Should include explicitly passed python file even if it is in excludes
    assert (
        foo_a
        in TargetManager(
            scanning_root_strings=frozenset([Path("."), Path("foo/a.py")]),
            includes=[],
            excludes={SAST_PRODUCT: ["foo/a.py"]},
        )
        .get_files_for_rule(python_language, [], [], "dummy_rule_id", SAST_PRODUCT)
        .targets
    )

    # Should ignore explicitly passed .go file when requesting python
    assert (
        TargetManager(
            scanning_root_strings=frozenset([Path("foo/a.go")]),
        )
        .get_files_for_rule(python_language, [], [], "dummy_rule_id", SAST_PRODUCT)
        .targets
        == frozenset()
    )

    # Should include explicitly passed file with unknown extension if allow_unknown_extensions=True
    assert_path_sets_equal(
        actual=TargetManager(
            scanning_root_strings=frozenset([Path("foo/noext")]),
            allow_unknown_extensions=True,
        )
        .get_files_for_rule(python_language, [], [], "dummy_rule_id", SAST_PRODUCT)
        .targets,
        expected={foo_noext},
    )

    # Should not include explicitly passed file with unknown extension by default
    assert_path_sets_equal(
        actual=TargetManager(
            scanning_root_strings=frozenset([Path("foo/noext")]),
        )
        .get_files_for_rule(python_language, [], [], "dummy_rule_id", SAST_PRODUCT)
        .targets,
        expected=set(),
    )

    # Should include explicitly passed file with correct extension even if skip_unknown_extensions=True
    assert_path_sets_equal(
        actual=TargetManager(
            scanning_root_strings=frozenset([Path("foo/noext"), Path("foo/a.py")]),
        )
        .get_files_for_rule(python_language, [], [], "dummy_rule_id", SAST_PRODUCT)
        .targets,
        expected={foo_a},
    )

    # Should respect includes/excludes passed to get_files even if target explicitly passed
    assert_path_sets_equal(
        actual=TargetManager(
            scanning_root_strings=frozenset([Path("foo/a.py"), Path("foo/b.py")]),
        )
        .get_files_for_rule(
            python_language, ["a.py"], [], "dummy_rule_id", SAST_PRODUCT
        )
        .targets,
        expected={foo_a},
    )

    # Should respect excludes on a per-product basis
    assert_path_sets_equal(
        actual=TargetManager(
            scanning_root_strings=frozenset([Path("foo/a.py"), Path("foo/b.py")]),
            excludes={SAST_PRODUCT: ["*.py"]},
        )
        .get_files_for_rule(
            python_language, ["a.py"], [], "dummy_rule_id", SECRETS_PRODUCT
        )
        .targets,
        expected={foo_a},
    )


@pytest.mark.quick
def test_unsupported_lang_paths(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)

    targets: List[Path] = []

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
            targets.append(path)
            if os.path.splitext(path)[1] != ".py":
                expected_unsupported.add(path)

    target_manager = TargetManager(
        scanning_root_strings=frozenset(targets),
    )

    target_manager.get_files_for_language(lang=LANG_PY, product=SAST_PRODUCT)
    target_manager.get_files_for_language(lang=LANG_GENERIC, product=SAST_PRODUCT)
    target_manager.get_files_for_language(lang=LANG_REGEX, product=SAST_PRODUCT)

    assert_path_sets_equal(
        actual=target_manager.ignore_log.unsupported_lang_paths(product=SAST_PRODUCT),
        expected=expected_unsupported,
    )


@pytest.mark.quick
def test_unsupported_lang_paths_2(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)

    targets: List[Path] = []

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
            targets.append(path)
            expected_unsupported.add(path)

    target_manager = TargetManager(
        scanning_root_strings=frozenset(targets),
    )

    target_manager.get_files_for_language(lang=LANG_GENERIC, product=SAST_PRODUCT)
    target_manager.get_files_for_language(lang=LANG_REGEX, product=SAST_PRODUCT)

    assert_path_sets_equal(
        actual=target_manager.ignore_log.unsupported_lang_paths(product=SAST_PRODUCT),
        expected=expected_unsupported,
    )


@pytest.mark.kinda_slow
def test_ignore_baseline_handler(monkeypatch, tmp_path):
    """
    Test verifies unchanged lockfiles are returned if ignore_baseline_handler=True,
    and only changed lockfiles are returned if ignore_baseline_handler=False
    """
    monkeypatch.chdir(tmp_path)

    # Initialize State
    subprocess.check_call(["git", "init"])
    subprocess.check_call(
        ["git", "config", "user.email", "baselinehandlertest@semgrep.com"]
    )
    subprocess.check_call(["git", "config", "user.name", "Baseline TestHandler"])
    subprocess.check_call(["git", "checkout", "-B", "main"])

    targets: List[Path] = []

    # Create dir_a/poetry.lock, dir_b/poetry.lock and dir_c/poetry.lock
    cwd = Path(".")
    targets.append(cwd)
    dir_a = Path("dir_a")
    dir_a.mkdir()
    dir_a_poetry = dir_a / "poetry.lock"
    dir_a_poetry.touch()
    dir_b = Path("dir_b")
    dir_b.mkdir()
    dir_b_poetry = dir_b / "poetry.lock"
    dir_b_poetry.touch()

    # Add and commit dir_a and dir_b
    subprocess.check_call(["git", "add", "."])
    subprocess.check_call(["git", "commit", "-m", "first"])
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Modify dir_b/poetry.lock and add dir_c/poetry.lock
    dir_b_poetry.write_text("#comment")
    dir_c = Path("dir_c")
    dir_c.mkdir()
    dir_c_poetry = dir_c / "poetry.lock"
    dir_c_poetry.touch()

    # Add and commit changes
    subprocess.check_call(["git", "add", "."])
    subprocess.check_call(["git", "commit", "-m", "second"])

    # Set up TargetManager
    baseline_handler = BaselineHandler(base_commit, True)
    target_manager = TargetManager(
        scanning_root_strings=frozenset(targets),
        baseline_handler=baseline_handler,
    )

    # Call get_files_for_language with ignore_baseline_handler=False
    # Should only return lockfiles in dir_b and dir_c as they were changed after base_commit
    diff_files = target_manager.get_files_for_language(
        lang=Ecosystem(Pypi()), product=SCA_PRODUCT, ignore_baseline_handler=False
    ).kept
    assert {str(dir_b_poetry), str(dir_c_poetry)} == {
        str(path) for path in diff_files
    }, "Should only include modified lockfiles"

    # Call get_files_for_language with ignore_baseline_handler=True
    # Should return all three lockfiles
    all_files = target_manager.get_files_for_language(
        lang=Ecosystem(Pypi()), product=SCA_PRODUCT, ignore_baseline_handler=True
    ).kept
    assert {str(dir_a_poetry), str(dir_b_poetry), str(dir_c_poetry)} == {
        str(path) for path in all_files
    }, "Should include unchanged lockfiles as well"
