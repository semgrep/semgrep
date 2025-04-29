import subprocess
from pathlib import Path
from typing import Dict
from typing import List

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.git import BaselineHandler
from semgrep.resolve_subprojects import filter_changed_subprojects
from semgrep.rule import Rule
from semgrep.subproject import get_all_source_files
from semgrep.target_manager import TargetManager

EXAMPLE_RULES = [
    Rule(
        {
            "id": "rules.ssc-58d96261-a0dd-47e9-bad6-110669fa8c14",
            "r2c-internal-project-depends-on": {
                "namespace": "pypi",
                "package": "protobuf",
                "version": ">=3.0.0",
            },
            "languages": ["python"],
            "patterns": ["pattern"],
        }
    ),
]


def make_subproject(
    manifest: Path, lockfile: Path, ecosystem: out.Ecosystem
) -> out.Subproject:
    return out.Subproject(
        root_dir=out.Fpath(str(manifest.parent)),
        # manifest and lockfile kind don't matter for this test
        dependency_source=out.DependencySource(
            out.ManifestLockfile(
                (
                    out.Manifest(
                        out.ManifestKind(out.PyprojectToml()), out.Fpath(str(manifest))
                    ),
                    out.Lockfile(
                        out.LockfileKind(out.PoetryLock()), out.Fpath(str(lockfile))
                    ),
                ),
            )
        ),
        ecosystem=ecosystem,
    )


@pytest.mark.kinda_slow
def test_without_baseline(monkeypatch: pytest.MonkeyPatch, tmp_path: Path):
    """
    Verify that all subprojects are marked as changed for a full scan

    If both change_filenames and create_filenames are None, no
    """
    monkeypatch.chdir(tmp_path)

    # Initialize git repo
    subprocess.check_call(["git", "init"])
    subprocess.check_call(["git", "config", "user.email", "baselinetest@semgrep.com"])
    subprocess.check_call(["git", "config", "user.name", "Baseline Test"])
    subprocess.check_call(["git", "checkout", "-B", "main"])

    # create baseline files for a python subproject
    foo = Path("foo")
    foo.mkdir()
    foo_a = foo / "pyproject.toml"
    foo_a.touch()
    foo_b = foo / "poetry.lock"
    foo_b.touch()

    # Add and commit
    subprocess.check_call(["git", "add", "."])
    subprocess.check_call(["git", "commit", "-m", "first"])

    # Set up TargetManager
    target_manager = TargetManager(
        scanning_root_strings=frozenset([Path(".")]),
    )

    subprojects = [make_subproject(foo_a, foo_b, out.Ecosystem(out.Pypi()))]

    relevant, irrelevant = filter_changed_subprojects(
        target_manager, EXAMPLE_RULES, subprojects
    )

    # expect the relevant to be exactly the same as all the subprojects
    assert len(irrelevant) == 0
    assert len(relevant) == len(subprojects)
    assert set(subprojects) == set(relevant)


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    ["baseline_subprojects", "new_filenames", "expected_changed_subprojects"],
    [
        (
            {
                "foo": make_subproject(
                    Path("foo/pyproject.toml"),
                    Path("foo/poetry.lock"),
                    out.Ecosystem(out.Pypi()),
                ),
                "bar": make_subproject(
                    Path("bar/pyproject.toml"),
                    Path("bar/poetry.lock"),
                    out.Ecosystem(out.Pypi()),
                ),
            },
            [Path("foo/example.py")],
            ["foo"],
        ),
        (
            {
                "foo": make_subproject(
                    Path("foo/pyproject.toml"),
                    Path("foo/poetry.lock"),
                    out.Ecosystem(out.Pypi()),
                ),
                "bar": make_subproject(
                    Path("bar/pyproject.toml"),
                    Path("bar/poetry.lock"),
                    out.Ecosystem(out.Pypi()),
                ),
            },
            [Path("foo/example.py"), Path("bar/example.py")],
            ["foo", "bar"],
        ),
        (
            {
                "foo": make_subproject(
                    Path("foo/pyproject.toml"),
                    Path("foo/poetry.lock"),
                    out.Ecosystem(out.Pypi()),
                ),
                "bar": make_subproject(
                    Path("bar/pyproject.toml"),
                    Path("bar/poetry.lock"),
                    out.Ecosystem(out.Pypi()),
                ),
            },
            [],
            [],
        ),
    ],
)
def test_with_baseline__new_code_files(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
    baseline_subprojects: Dict[str, out.Subproject],
    new_filenames: List[Path],
    expected_changed_subprojects: List[str],
):
    """
    Verify that the correct subprojects are marked as changed for a diff scan when new code files are added.

    `baseline_subprojects` is a map from some name to a subproject. This name should be used to refer back to the subproject
    in `expected_changed_subprojects`.

    `baseline_subprojects` lists subprojects that should exist in the baseline scan. The dependency source files
    in these subprojects will be created in the baseline commit.

    `new_filenames` lists filenames that should be created on the branch that the diff scan is run on.
    """
    monkeypatch.chdir(tmp_path)

    # Initialize git repo
    subprocess.check_call(["git", "init"])
    subprocess.check_call(["git", "config", "user.email", "baselinetest@semgrep.com"])
    subprocess.check_call(["git", "config", "user.name", "Baseline Test"])
    subprocess.check_call(["git", "checkout", "-B", "main"])

    # create dependency source files for all baseline subprojects
    for _key, subproject in baseline_subprojects.items():
        for source_file in get_all_source_files(subproject.dependency_source):
            source_file.parent.mkdir(parents=True, exist_ok=True)
            source_file.touch()

    # Add and commit baseline dependency source files
    subprocess.check_call(["git", "add", "."])
    subprocess.check_call(["git", "commit", "-m", "first"])
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # create any new dependency source files in the diff commit
    for new_filename in new_filenames:
        new_filename.parent.mkdir(parents=True, exist_ok=True)
        new_filename.touch()

    # Add and commit all changes
    subprocess.check_call(["git", "add", "."])
    # baseline_handler works on pending changes
    baseline_handler = BaselineHandler(base_commit)

    # Set up TargetManager
    baseline_handler = BaselineHandler(base_commit, True)
    target_manager = TargetManager(
        scanning_root_strings=frozenset([Path(".")]),
        baseline_handler=baseline_handler,
    )

    relevant, irrelevant = filter_changed_subprojects(
        target_manager, EXAMPLE_RULES, list(baseline_subprojects.values())
    )

    # expect the set of subprojects deemed relevant to match the keys given in expected_changed_subprojects
    expected_subprojects = [
        baseline_subprojects[key] for key in expected_changed_subprojects
    ]
    assert len(relevant) == len(expected_subprojects)
    assert set(relevant) == set(expected_subprojects)
    assert len(relevant) + len(irrelevant) == len(baseline_subprojects)


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    ["baseline_subprojects", "changed_filenames", "expected_changed_subprojects"],
    [
        (
            {
                "foo": make_subproject(
                    Path("foo/pyproject.toml"),
                    Path("foo/poetry.lock"),
                    out.Ecosystem(out.Pypi()),
                ),
                "bar": make_subproject(
                    Path("bar/pyproject.toml"),
                    Path("bar/poetry.lock"),
                    out.Ecosystem(out.Pypi()),
                ),
            },
            [Path("foo/pyproject.toml")],
            ["foo"],
        ),
        (
            {
                "foo": make_subproject(
                    Path("foo/pyproject.toml"),
                    Path("foo/poetry.lock"),
                    out.Ecosystem(out.Pypi()),
                ),
                "bar": make_subproject(
                    Path("bar/pyproject.toml"),
                    Path("bar/poetry.lock"),
                    out.Ecosystem(out.Pypi()),
                ),
            },
            [Path("foo/poetry.lock")],
            ["foo"],
        ),
        (
            {
                "foo": make_subproject(
                    Path("foo/pyproject.toml"),
                    Path("foo/poetry.lock"),
                    out.Ecosystem(out.Pypi()),
                ),
                "bar": make_subproject(
                    Path("bar/pyproject.toml"),
                    Path("bar/poetry.lock"),
                    out.Ecosystem(out.Pypi()),
                ),
            },
            [Path("foo/pyproject.toml"), Path("bar/poetry.lock")],
            ["foo", "bar"],
        ),
        (
            {
                "foo": make_subproject(
                    Path("foo/pyproject.toml"),
                    Path("foo/poetry.lock"),
                    out.Ecosystem(out.Pypi()),
                ),
                "bar": make_subproject(
                    Path("bar/pyproject.toml"),
                    Path("bar/poetry.lock"),
                    out.Ecosystem(out.Pypi()),
                ),
            },
            [],
            [],
        ),
    ],
)
def test_with_baseline__changed_source_files(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
    baseline_subprojects: Dict[str, out.Subproject],
    changed_filenames: List[Path],
    expected_changed_subprojects: List[str],
):
    """
    Verify that the correct subprojects are marked as changed for a diff scan when new code files are added.

    `baseline_subprojects` is a map from some name to a subproject. This name should be used to refer back to the subproject
    in `expected_changed_subprojects`.

    `baseline_subprojects` lists subprojects that should exist in the baseline scan. The dependency source files
    in these subprojects will be created in the baseline commit.

    `new_filenames` lists filenames that should be created on the branch that the diff scan is run on.
    """
    monkeypatch.chdir(tmp_path)

    # Initialize git repo
    subprocess.check_call(["git", "init"])
    subprocess.check_call(["git", "config", "user.email", "baselinetest@semgrep.com"])
    subprocess.check_call(["git", "config", "user.name", "Baseline Test"])
    subprocess.check_call(["git", "checkout", "-B", "main"])

    # create dependency source files for all baseline subprojects
    for _key, subproject in baseline_subprojects.items():
        for source_file in get_all_source_files(subproject.dependency_source):
            source_file.parent.mkdir(parents=True, exist_ok=True)
            source_file.touch()

    # Add and commit baseline dependency source files
    subprocess.check_call(["git", "add", "."])
    subprocess.check_call(["git", "commit", "-m", "first"])
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # create any new dependency source files in the diff commit
    for change_filename in changed_filenames:
        with open(change_filename, "w") as f:
            f.write("blahblah")

    # Add and commit all changes
    subprocess.check_call(["git", "add", "."])
    # baseline_handler works on pending changes
    baseline_handler = BaselineHandler(base_commit)

    # Set up TargetManager
    baseline_handler = BaselineHandler(base_commit, True)
    target_manager = TargetManager(
        scanning_root_strings=frozenset([Path(".")]),
        baseline_handler=baseline_handler,
    )

    relevant, irrelevant = filter_changed_subprojects(
        target_manager, EXAMPLE_RULES, list(baseline_subprojects.values())
    )
    print("relevant subprojects", relevant)
    print("irrelevant subprojects", irrelevant)

    # expect the set of subprojects deemed relevant to match the keys given in expected_changed_subprojects
    expected_subprojects = [
        baseline_subprojects[key] for key in expected_changed_subprojects
    ]
    assert len(relevant) == len(expected_subprojects)
    assert set(relevant) == set(expected_subprojects)
    assert len(relevant) + len(irrelevant) == len(baseline_subprojects)
