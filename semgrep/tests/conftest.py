import contextlib
import json
import os
import shutil
import subprocess
from pathlib import Path
from typing import List
from typing import Optional
from typing import Union

import appdirs
import pytest

from . import public_repos

TESTS_PATH = Path(__file__).parent

MASKED_KEYS = [
    "tool.driver.semanticVersion",
    "results.extra.metavars.*.unique_id.md5sum",
]


def mark_masked(obj, path):
    _mark_masked(obj, path.split("."))


def _mark_masked(obj, path_items):
    key = path_items[0]
    if len(path_items) == 1 and key in obj:
        obj[key] = "<masked in tests>"
    else:
        if key == "*":
            next_obj = list(obj.values())
        else:
            next_obj = obj.get(key)
        if next_obj is None:
            next_objs = []
        elif not isinstance(next_obj, list):
            next_objs = [next_obj]
        else:
            next_objs = next_obj
        for o in next_objs:
            _mark_masked(o, path_items[1:])


def _clean_output_json(output_json: str) -> str:
    """Make semgrep's output deterministic and nicer to read."""
    output = json.loads(output_json)
    for path in MASKED_KEYS:
        mark_masked(output, path)

    return json.dumps(output, indent=2, sort_keys=True)


def _run_semgrep(
    config: Optional[Union[str, Path, List[str]]] = None,
    *,
    target_name: str = "basic",
    options: Optional[List[Union[str, Path]]] = None,
    output_format: str = "json",
    stderr: bool = False,
    strict: bool = True,
) -> str:
    """Run the semgrep CLI.

    :param config: what to pass as --config's value
    :param target_name: which directory within ./e2e/targets/ to scan
    :param options: additional CLI flags to add
    :param output_format: which format to use, valid options are normal, json, junit_xml and sarif
    :param stderr: whether to merge stderr into the returned string
    """
    if options is None:
        options = []

    if strict:
        options.append("--strict")

    options.append("--disable-version-check")

    if config is not None:
        if isinstance(config, list):
            for conf in config:
                options.extend(["--config", conf])
        else:
            options.extend(["--config", config])

    if output_format == "json":
        options.append("--json")
    elif output_format == "junit-xml":
        options.append("--junit-xml")
    elif output_format == "sarif":
        options.append("--sarif")

    output = subprocess.check_output(
        ["python3", "-m", "semgrep", *options, Path("targets") / target_name],
        encoding="utf-8",
        stderr=subprocess.STDOUT if stderr else subprocess.PIPE,
    )

    if output_format in {"json", "sarif"} and not stderr:
        output = _clean_output_json(output)

    return output


REPO_CACHE = Path(
    os.path.expanduser(
        os.environ.get("GITHUB_REPO_CACHE", appdirs.user_cache_dir("semgrep-tests"))
    )
)


@pytest.fixture()
def clone_github_repo():
    """
    Fixture to clone a github repo. Usage:
    ```
    def my_test_function(clone_github_repo):
        repo_path = clone_github_repo(url="https://github.com/returntocorp/semgrep", sha="abdfe')
        subprocess.run(["ls", repo_path])
    ```

    :returns: A path to the repo, guaranteed to live at least until the end of the test
    """
    yield _github_repo_retry_wrapper


@contextlib.contextmanager
def chdir(dirname=None):
    curdir = os.getcwd()
    try:
        if dirname is not None:
            os.chdir(dirname)
        yield
    finally:
        os.chdir(curdir)


def _github_repo_retry_wrapper(
    repo_url: str, sha: Optional[str] = None, retries: int = 3
):
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
            return _github_repo_retry_wrapper(repo_url, sha, retries - 1)


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
                    f"Github repo is broken (not set to correct sha: {repo_sha.decode('utf-8')}"
                )

    return repo_destination


@pytest.fixture
def run_semgrep_in_tmp(monkeypatch, tmp_path):
    (tmp_path / "targets").symlink_to(Path(TESTS_PATH / "e2e" / "targets").resolve())
    (tmp_path / "rules").symlink_to(Path(TESTS_PATH / "e2e" / "rules").resolve())

    monkeypatch.chdir(tmp_path)

    yield _run_semgrep


def pytest_addoption(parser):
    parser.addoption(
        "--qa",
        action="store_true",
        dest="is_qa",
        default=False,
        help="enable comprehensive QA tests",
    )


def pytest_configure(config):
    config.addinivalue_line("markers", "qa: mark tests that only need to run during QA")


def pytest_runtest_setup(item):
    if item.get_closest_marker("qa") and not item.config.getoption("--qa"):
        pytest.skip("skipping QA tests, add --qa flag to run them")


# Add `public_repo_url` as a parameter to your test to use this. It generates 1 test case per repo url in `ALL_REPOS`
def pytest_generate_tests(metafunc):
    if "public_repo_url" in metafunc.fixturenames:
        repos = public_repos.ALL_REPOS
        metafunc.parametrize("public_repo_url", repos, ids=lambda r: r["repo"])
