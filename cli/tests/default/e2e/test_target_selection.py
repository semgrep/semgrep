#
# Test target selection before any rule or language-specific filtering
#
import os
import shutil
import sys
from dataclasses import dataclass
from dataclasses import field
from pathlib import Path
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

import pytest
from tests.fixtures import RunSemgrep


# The expectations regarding a particular target file path
@dataclass
class Expect:
    selected: bool
    selected_by_pysemgrep: Optional[bool] = None
    selected_by_osemgrep: Optional[bool] = None
    ignore_pysemgrep_result: bool = False
    ignore_osemgrep_result: bool = False
    paths: List[str] = field(default_factory=lambda: [])


# Check whether a target path was selected or ignored by semgrep, depending
# the expectation we have.
def check_expectation(
    expect: Expect,
    is_running_osemgrep: bool,
    is_git_project: bool,
    selected_targets: Set[str],
):
    paths = expect.paths

    if is_running_osemgrep and expect.ignore_osemgrep_result:
        return
    if not is_running_osemgrep and expect.ignore_pysemgrep_result:
        return

    expect_selected = expect.selected
    if is_running_osemgrep and expect.selected_by_osemgrep is not None:
        expect_selected = expect.selected_by_osemgrep
    if not is_running_osemgrep and expect.selected_by_pysemgrep is not None:
        expect_selected = expect.selected_by_pysemgrep

    label = "[osemgrep]" if is_running_osemgrep else "[pysemgrep]"
    label = label + (" [git project]" if is_git_project else " [nongit project]")
    for path in paths:
        if expect_selected:
            print(
                f"{label} check that target path was selected: {path}", file=sys.stderr
            )
            assert path in selected_targets
        else:
            print(
                f"{label} check that target path was ignored: {path}", file=sys.stderr
            )
            assert path not in selected_targets


# What we expect from semgrep when running with the most common invocation i.e.
#
# - run semgrep from the project root
# - run semgrep on the project root
# - no optional command-line flags
# - shared expectations in git and nongit projects
#
# To cover a new test case, add a file to the test repo and specify
# the expectations for the new path below.
#
COMMON_EXPECTATIONS = [
    Expect(
        selected=True,
        paths=[
            # Paths that are correctly selected by both pysemgrep and osemgrep
            ".gitignore",
            ".gitmodules",
            ".semgrepignore",
            "README.md",
            "gitignored-only-in-src-and-below.py",
            "gitignored-only-in-src.py",
            "hello.py",
            "img/hello.svg",
            "semgrepignored/hello.py",
            "semgrepignored-folder",
            "semgrepignored-only-in-src-and-below.py",
            "semgrepignored-only-in-src.py",
            "sempignored-py-contents/hello.rb",
            "src/.gitignore",
            "src/.hidden.py",
            "src/.semgrepignore",
            "src/10KiB.py",
            "src/hello.py",
            "src/semgrepignored-root",
            "src/semgrepignored-root-folder/hello.py",
            "src/sempignored-py-contents/hello.rb",
            "src/src/semgrepignored-anchored/hello.py",
            "src/subdir/gitignored-only-in-src.py",
            "src/subdir/semgrepignored-only-in-src.py",
            "tests/hello.py",
            # special characters in file names
            "src/~",
            "src/quote'/hello.py",
            "src/space !/hello.py",
            "src/🚀.py",
        ],
    ),
    Expect(
        selected=False,
        paths=[
            # Paths that are correctly ignored by both pysemgrep and osemgrep
            "img/red.png",
            "semgrepignored-everywhere/hello.py",
            "semgrepignored-root",
            "semgrepignored-root-folder/hello.py",
            "src/broken-symlink.py",
            "src/semgrepignored-everywhere/hello.py",
            "src/semgrepignored-folder/hello.py",
            "src/sempignored-py-contents/hello.py",
            "src/symlink.py",
        ],
    ),
    # accepted differences between pysemgrep and osemgrep
    Expect(
        # excluded by osemgrep, selected by pysemgrep
        selected=False,
        selected_by_pysemgrep=True,
        paths=[
            # pysemgrep doesn't consult .semgrepignore files in subfolders:
            "src/semgrepignored-only-in-src-and-below.py",
            "src/semgrepignored-only-in-src.py",
        ],
    ),
    # pysemgrep bugs
    Expect(
        selected=False,
        selected_by_pysemgrep=True,
        paths=["sempignored-py-contents/hello.py"],
    ),
]

GIT_PROJECT_EXPECTATIONS = [
    # common expectations for a git project (but not for a nongit project)
    Expect(
        selected=False,
        paths=[
            # git submodule object (folder) listed by 'git ls-files'
            "submodules/semgrep-test-project2",
            # git submodule contents
            "submodules/semgrep-test-project2/hello.py",
        ],
    ),
    # accepted differences between pysemgrep and osemgrep
    Expect(
        selected=False,
        selected_by_pysemgrep=True,
        paths=[
            # pysemgrep doesn't consult .gitignore files
            "src/gitignored.py",
            "src/gitignored-only-in-src-and-below.py",
            "src/gitignored-only-in-src.py",
        ],
    ),
]

NONGIT_PROJECT_EXPECTATIONS = [
    # common expectations for a nongit project (but not for a git project)
    Expect(
        selected=True,
        paths=[
            # regular file in what was a git submodule
            "submodules/semgrep-test-project2/hello.py",
            # we don't consult .gitignore files in nongit projects
            "src/gitignored.py",
            "src/gitignored-only-in-src-and-below.py",
            "src/gitignored-only-in-src.py",
        ],
    ),
    Expect(
        selected=False,
        paths=[
            # folder, not a regular file
            "submodules/semgrep-test-project2",
        ],
    ),
]

# This is an artificial git project that offers all the difficulties we could
# think of for file targeting.
#
# Do we need to run on several projects?
PROJECT: Tuple[str, str] = (
    "semgrep-test-project1",
    "https://github.com/semgrep/semgrep-test-project1.git",
)


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    # a list of extra semgrep CLI options and osemgrep-specific options
    "is_git_project,options,osemgrep_options,expectations",
    [
        (True, [], [], COMMON_EXPECTATIONS + GIT_PROJECT_EXPECTATIONS),
        (False, [], [], COMMON_EXPECTATIONS + NONGIT_PROJECT_EXPECTATIONS),
    ],
    ids=["git", "nongit"],
)
def test_project_target_selection(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
    run_semgrep: RunSemgrep,
    is_git_project: bool,
    options: List[str],
    osemgrep_options: List[str],
    expectations: List[Expect],
) -> None:
    # Instead of copying or symlinking the git submodule that sits nicely
    # in our semgrep repo, we clone it as standalone repo to avoid problems
    # due to having the structure of a submodule but no parent git project.
    project_name, project_url = PROJECT
    print(f"cd into {tmp_path}", file=sys.stderr)
    monkeypatch.chdir(tmp_path)
    print(f"clone {project_url}", file=sys.stderr)
    os.system(f"git clone {project_url} {project_name}")
    print(f"cd into {project_name}", file=sys.stderr)
    monkeypatch.chdir(Path(project_name))
    print(f"check out submodules", file=sys.stderr)
    os.system(f"git submodule update --init --recursive")

    if not is_git_project:
        print(f"remove .git to make this a non-git project", file=sys.stderr)
        shutil.rmtree(".git")

    is_running_osemgrep = True if os.environ.get("PYTEST_USE_OSEMGREP") else False

    extra_options = options
    if is_running_osemgrep:
        extra_options += osemgrep_options

    # Call semgrep to list the target files and print them on stdout,
    # one per line.
    stdout, stderr = run_semgrep(
        # the '-e' and '--lang' options are to keep pysemgrep happy because
        # it wants to load rules
        options=["--x-ls", "-e", "hello", "--lang", "python"] + extra_options,
        assume_targets_dir=False,
        target_name=".",
    )
    selected_targets: Set[str] = set(filter(lambda x: x, stdout.split("\n")))

    print(f"selected target paths:", file=sys.stderr)
    for path in sorted(list(selected_targets)):
        print(f"  {path}", file=sys.stderr)

    # Check the status of each file path we want to check.
    for expect in expectations:
        check_expectation(expect, is_running_osemgrep, is_git_project, selected_targets)
