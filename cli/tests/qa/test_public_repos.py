#
# TODO: explain what these tests are for if you happen to know.
#
import contextlib
import json
import os
import shutil
import subprocess
from pathlib import Path

import appdirs
import pytest
from tests.conftest import TESTS_PATH
from tests.semgrep_runner import SEMGREP_BASE_SCAN_COMMAND

from .public_repos import REPOS

# Some improbable string that was implanted in test targets [how?] [why?].
#

SENTINEL_VALUE = 87518275812375164

LANGUAGE_SENTINELS = {
    "python": {
        "filename": "sentinel.py",
        "file_contents": f"sentinel = {SENTINEL_VALUE}",
    },
    "go": {
        "filename": "sentinel.go",
        "file_contents": f"package Sentinel\nconst sentinel = {SENTINEL_VALUE}",
    },
    "javascript": {
        "filename": "sentinel.js",
        "file_contents": f"sentinel = {SENTINEL_VALUE}",
    },
    "ruby": {
        "filename": "sentinel.rb",
        "file_contents": f"sentinel = {SENTINEL_VALUE}",
    },
}
SENTINEL_PATTERN = f"$SENTINEL = {SENTINEL_VALUE}"


@contextlib.contextmanager
def chdir(dirname=None):
    curdir = os.getcwd()
    try:
        if dirname is not None:
            os.chdir(dirname)
        yield
    finally:
        os.chdir(curdir)


def assert_sentinel_results(repo_path, sentinel_path, language):
    cmd = SEMGREP_BASE_SCAN_COMMAND + [
        "--disable-version-check",
        "--pattern",
        SENTINEL_PATTERN,
        "--lang",
        language,
        "--json",
        repo_path,
        "--metrics=off",
        # Turn off optimizations since it skips parsing when it can and this test is testing parsing
        "--optimizations=none",
    ]

    # This is useful for debugging. I don't see a downside to printing
    # such important debugging info when running tests so I'm leaving it
    # -- Martin
    print(f"semgrep command: {cmd}")

    # nosemgrep: python.lang.security.audit.dangerous-subprocess-use.dangerous-subprocess-use
    semgrep_run = subprocess.run(cmd, capture_output=True, encoding="utf-8")
    assert semgrep_run.returncode == 0

    output = json.loads(semgrep_run.stdout)
    assert output["errors"] == []
    assert len(output["results"]) == 1
    assert output["results"][0]["path"] == str(sentinel_path)


REPO_CACHE = Path(
    os.environ.get("QA_TESTS_CACHE_PATH", appdirs.user_cache_dir("semgrep-qa-tests"))
)


def clone_github_repo(repo_url: str, retries: int = 3):
    """
    Internal fixture function. Do not use directly, use the `clone_github_repo` fixture.
    Wraps `_github_repo` function with retries. If the `_github_repo` throws an exception,
    it will delete `repo_destination` and retry up to `retries` times.
    """
    repo_dir = "-".join(repo_url.split("/")[-2:])
    repo_destination = REPO_CACHE / repo_dir
    try:
        return _github_repo(repo_url, repo_destination)
    except (GitError, subprocess.CalledProcessError) as ex:
        print(f"Failed to clone github repo for tests {ex}")
        if repo_destination.exists():
            shutil.rmtree(repo_destination)
        if retries == 0:
            raise
        else:
            return clone_github_repo(repo_url, retries - 1)


class GitError(Exception):
    pass


def _github_repo(repo_url: str, repo_destination: Path):
    """
    Internal fixture function. Use the `clone_github_repo` fixture.
    Clones the github repo at repo_url into `repo_destination` and checks out `sha`.

    If `repo_destination` already exists, it will validate that the correct repo is present at that location.
    """
    if not repo_destination.exists():
        subprocess.check_output(
            ["git", "clone", "--depth=1", repo_url, repo_destination]
        )

    # validate that the repo seems setup properly
    with chdir(repo_destination):
        # some tests modify it, lets put everything back to normal
        subprocess.check_output(["git", "clean", "-fd"])
        subprocess.check_output(["git", "reset", "--hard"])
        assert (
            subprocess.check_output(["git", "status", "--porcelain"]).strip() == b""
        ), "repo must be clean"

    return repo_destination


@pytest.mark.slow
@pytest.mark.parametrize("repo", [repo.as_param() for repo in REPOS])
#
# This test runs [which checks?] against one public git repo.
# See the list of repos in public_repos.py.
# Those repos, once downloaded are cached locally in ~/.cache.
# You may have to clear the cache manually if some tests start failing.
#
def test_semgrep_on_repo(monkeypatch, tmp_path, repo):
    (tmp_path / "rules").symlink_to(Path(TESTS_PATH / "qa" / "rules").resolve())

    monkeypatch.chdir(tmp_path)

    repo_path = clone_github_repo(repo_url=repo.url)
    repo_languages = (
        LANGUAGE_SENTINELS
        if repo.languages is None
        else {
            language: sentinel_info
            for language, sentinel_info in LANGUAGE_SENTINELS.items()
            if language in repo.languages
        }
    )

    for language, sentinel_info in repo_languages.items():
        sentinel_path = repo_path / sentinel_info["filename"]
        with sentinel_path.open("w") as sentinel_file:
            sentinel_file.write(sentinel_info["file_contents"])
        assert_sentinel_results(repo_path, sentinel_path, language)

    cmd = SEMGREP_BASE_SCAN_COMMAND + [
        "--disable-version-check",
        "--config=rules/regex-sentinel.yaml",
        "--strict",
        "--json",
        "--metrics=off",
        # Turn off optimizations since it skips parsing when it can and this test is testing parsing
        "--optimizations=none",
        repo_path,
    ]

    print(f"semgrep command: {cmd}")

    # nosemgrep: python.lang.security.audit.dangerous-subprocess-use.dangerous-subprocess-use
    res = subprocess.run(cmd, encoding="utf-8", capture_output=True)
    print("--- semgrep error output ---")
    print(res.stderr)
    print("----------------------------")
    print("--- semgrep standard output ---")
    print(res.stdout)
    print("-------------------------------")
    assert res.returncode == 0

    output = json.loads(res.stdout)
    assert output["results"]
    assert len(output["results"]) == len(repo_languages)
    assert output["errors"] == []
