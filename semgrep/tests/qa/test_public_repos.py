import json
import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Optional

import appdirs
import pytest

from ..conftest import chdir
from ..conftest import TESTS_PATH
from ..public_repos import ALL_LANGUAGES
from ..public_repos import ALL_REPOS


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


def _assert_sentinel_results(
    repo_url, repo_path, sentinel_path, language, excludes=None
):
    """
    EXCLUDES: Optional[List[str]] : Each element is passed to semgrep cli as --exclude flag
    """
    cmd = [
        sys.executable,
        "-m",
        "semgrep",
        "--pattern",
        SENTINEL_PATTERN,
        "--lang",
        language,
        "--json",
        repo_path,
        "--optimizations",
        "none",  # Turn off optimizations since it skips parsing when it can and this test is testing parsing
    ]

    if excludes:
        for exclude in excludes:
            cmd.extend(["--exclude", exclude])

    semgrep_run = subprocess.run(
        cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        encoding="utf-8",
    )

    assert semgrep_run.returncode == 0
    try:
        output = json.loads(semgrep_run.stdout)
    except json.JSONDecodeError:
        pytest.fail(
            f"Failed to parse JSON from semgrep output ({semgrep_run.args}):\n"
            + semgrep_run.stdout
            + semgrep_run.stderr
        )

    if output["errors"]:
        pytest.fail(
            f"Running on {repo_url} with lang {language} had errors ({semgrep_run.args}): "
            + json.dumps(output["errors"], indent=4)
        )

    if len(output["results"]) != 1 or output["results"][0]["path"] != str(
        sentinel_path
    ):
        pytest.fail(
            f"Running on {repo_url} with lang {language} expected to have one results instead found result ({semgrep_run.args}): "
            + json.dumps(output["results"], indent=4)
        )


REPO_CACHE = Path(
    os.path.expanduser(
        os.environ.get("GITHUB_REPO_CACHE", appdirs.user_cache_dir("semgrep-tests"))
    )
)


def clone_github_repo(repo_url: str, sha: Optional[str] = None, retries: int = 3):
    """
    Internal fixture function. Do not use directly, use the `clone_github_repo` fixture.
    Wraps `_github_repo` function with retries. If the `_github_repo` throws an exception,
    it will delete `repo_destination` and retry up to `retries` times.
    """
    sha_str = sha or "latest"
    repo_dir = "-".join(repo_url.split("/")[-2:]) + "-" + sha_str
    repo_destination = REPO_CACHE / repo_dir
    try:
        return _github_repo(repo_url, sha, repo_destination)
    except (GitError, subprocess.CalledProcessError) as ex:
        print(f"Failed to clone github repo for tests {ex}")
        if repo_destination.exists():
            shutil.rmtree(repo_destination)
        if retries == 0:
            raise
        else:
            return clone_github_repo(repo_url, sha, retries - 1)


class GitError(BaseException):
    pass


def _github_repo(repo_url: str, sha: Optional[str], repo_destination: Path):
    """
    Internal fixture function. Use the `clone_github_repo` fixture.
    Clones the github repo at repo_url into `repo_destination` and checks out `sha`.

    If `repo_destination` already exists, it will validate that the correct repo is present at that location.
    """
    if not repo_destination.exists():
        if sha is None:
            subprocess.check_output(
                ["git", "clone", "--depth=1", repo_url, repo_destination]
            )
        else:
            repo_destination.mkdir()
            # Sadly, no fast way to clone a specific commit without a super
            # modern git client
            subprocess.check_output(["git", "clone", repo_url, repo_destination])
            with chdir(repo_destination):
                subprocess.check_output(["git", "checkout", sha])

    # validate that the repo seems setup properly
    with chdir(repo_destination):
        # some tests modify it, lets put everything back to normal
        subprocess.check_output(["git", "clean", "-fd"])
        subprocess.check_output(["git", "reset", "--hard"])
        all_clean = (
            subprocess.check_output(["git", "status", "--porcelain"]).strip() == b""
        )
        if not all_clean:
            raise GitError("Couldn't clean the repo, something is wrong. Deleting.")
        repo_sha = subprocess.check_output(["git", "rev-parse", "HEAD"])
        if sha:
            if not repo_sha.startswith(sha.encode("utf-8")):
                shutil.rmtree(repo_destination)
                raise GitError(
                    f"Github repo is broken (not set to correct sha: {repo_sha.decode('utf-8', errors='replace')}"
                )

    return repo_destination


@pytest.mark.parametrize("repo_object", ALL_REPOS)
# public_repo_url is a fancy dynamic parameterization defined in conftest.
def test_semgrep_on_repo(monkeypatch, tmp_path, repo_object):
    (tmp_path / "rules").symlink_to(Path(TESTS_PATH / "qa" / "rules").resolve())

    monkeypatch.chdir(tmp_path)

    repo_url = repo_object["repo"]
    languages = repo_object["languages"]
    excludes = repo_object.get("excludes")
    repo_path = clone_github_repo(repo_url=repo_url)
    repo_languages = (
        LANGUAGE_SENTINELS
        if languages == ALL_LANGUAGES
        else {
            language: sentinel_info
            for language, sentinel_info in LANGUAGE_SENTINELS.items()
            if language in languages
        }
    )

    for language, sentinel_info in repo_languages.items():
        sentinel_path = repo_path / sentinel_info["filename"]
        with sentinel_path.open("w") as sentinel_file:
            sentinel_file.write(sentinel_info["file_contents"])

        _assert_sentinel_results(repo_url, repo_path, sentinel_path, language, excludes)

    cmd = [
        sys.executable,
        "-m",
        "semgrep",
        "--config=rules/regex-sentinel.yaml",
        "--strict",
        "--json",
        "--optimizations",
        "none",  # Turn off optimizations since it skips parsing when it can and this test is testing parsing
        repo_path,
    ]

    if excludes:
        for exclude in excludes:
            cmd.extend(["--exclude", exclude])

    print(f"semgrep command: {cmd}")

    res = subprocess.run(
        cmd,
        encoding="utf-8",
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    returncode = res.returncode
    print("--- semgrep error output ---")
    print(res.stderr)
    print("----------------------------")
    print("--- semgrep standard output ---")
    print(res.stdout)
    print("-------------------------------")
    if returncode != 0:
        # Fail regardless of the expected status "ok" or "xfail".
        raise subprocess.SubprocessError(
            f"Semgrep exited with non-zero status: {returncode}"
        )
    output = json.loads(res.stdout)

    expected_results_count = len(repo_languages)
    if len(output["results"]) != expected_results_count or len(output["errors"]) != 0:
        pytest.fail(
            f"Running on {repo_url} with regex rules. Expect {expected_results_count} results and no errors but got: "
            + json.dumps(output, indent=4)
        )
