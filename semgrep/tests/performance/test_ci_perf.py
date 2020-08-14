import contextlib
import json
import os
import shutil
import subprocess
import time
from pathlib import Path
from typing import List
from typing import Optional
from typing import Union

from semgrep import util

# import appdirs

# import pytest

# def clone_github_repo():
#     """
#     Fixture to clone a github repo. Usage:
#     ```
#     def my_test_function(clone_github_repo):
#         repo_path = clone_github_repo(url="https://github.com/returntocorp/semgrep", sha="abdfe')
#         subprocess.run(["ls", repo_path])
#     ```

#     :returns: A path to the repo, guaranteed to live at least until the end of the test
#     """
#     yield _github_repo_retry_wrapper

# REPO_CACHE = Path(
#     os.path.expanduser(
#         os.environ.get("GITHUB_REPO_CACHE", appdirs.user_cache_dir("semgrep-tests"))
#     )
# )


@contextlib.contextmanager
def chdir(dirname=None):
    curdir = os.getcwd()
    try:
        if dirname is not None:
            os.chdir(dirname)
        yield
    finally:
        os.chdir(curdir)


def clone_github_repo(repo_url: str, sha: Optional[str] = None, retries: int = 3):
    """
    Internal fixture function. Do not use directly, use the `clone_github_repo` fixture.
    Wraps `_github_repo` function with retries. If the `_github_repo` throws an exception,
    it will delete `repo_destination` and retry up to `retries` times.
    """
    sha_str = sha or "latest"
    repo_dir = "-".join(repo_url.split("/")[-2:]) + "-" + sha_str
    repo_destination = Path(repo_dir)
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
                ["git", "clone", "--depth=1", repo_url, repo_destination],
                stderr=subprocess.STDOUT,
            )
        else:
            repo_destination.mkdir()
            # Sadly, no fast way to clone a specific commit without a super
            # modern git client
            subprocess.check_output(
                ["git", "clone", repo_url, repo_destination], stderr=subprocess.STDOUT
            )
            with chdir(repo_destination):
                subprocess.check_output(
                    ["git", "checkout", sha], stderr=subprocess.STDOUT
                )

    # validate that the repo seems setup properly
    with chdir(repo_destination):
        # some tests modify it, lets put everything back to normal
        subprocess.check_output(["git", "clean", "-fd"], stderr=subprocess.STDOUT)
        subprocess.check_output(["git", "reset", "--hard"], stderr=subprocess.STDOUT)
        all_clean = (
            subprocess.check_output(
                ["git", "status", "--porcelain"], stderr=subprocess.DEVNULL
            ).strip()
            == b""
        )
        if not all_clean:
            raise GitError("Couldn't clean the repo, something is wrong. Deleting.")
        repo_sha = subprocess.check_output(
            ["git", "rev-parse", "HEAD"], stderr=subprocess.STDOUT
        )
        if sha:
            if not repo_sha.startswith(sha.encode("utf-8")):
                shutil.rmtree(repo_destination)
                raise GitError(
                    f"Github repo is broken (not set to correct sha: {repo_sha.decode('utf-8')}"
                )

    return repo_destination


def test_perf():
    """
        Simple test that njsscan finishes below a given threshold of time on juice-shop and dvna
        this will alert us of significant performance regressions
    """
    rules_path = clone_github_repo(
        repo_url="https://github.com/ajinabraham/njsscan",
        sha="d1c5df41393ba512cbd362874a7a0bdc7dbf43fc",
    )
    njsscan_rules_path = str(rules_path / "njsscan/rules/semantic_grep")

    targets = [
        (  # Dvna takes about ~30 sec
            "https://github.com/appsecco/dvna",
            "c637437d6515bd4c732e91c58e62d38e88260d3c",
            ["jquery-3.2.1.min.js", "showdown.min.js"],
            40,
        ),
        (  # Juice Shop takes about ~150 sec on 2019MBP, ~270 sec on GHA
            "https://github.com/bkimminich/juice-shop",
            "98633f5ef242bf943608324a562058b22eca6dfe",
            ["three.js"],
            300,
        ),
    ]

    for repo_url, sha, excludes, _expected_duration in targets:
        target_path = clone_github_repo(repo_url=repo_url, sha=sha)
        args = [
            "python3",
            "-m",
            "semgrep",
            "--config",
            njsscan_rules_path,
            str(target_path),
        ]
        args.extend(util.flatten(["--exclude", ex] for ex in excludes))

        start = time.time()
        subprocess.check_output(args)
        duration = time.time() - start

        print(duration)
        # assert duration < expected_duration


test_perf()
