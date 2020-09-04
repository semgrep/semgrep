import json
import subprocess
from pathlib import Path

import pytest

from ..conftest import TESTS_PATH
from ..public_repos import ALL_LANGUAGES

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


def _assert_sentinel_results(repo_url, repo_path, sentinel_path, language):
    semgrep_run = subprocess.run(
        [
            "python3",
            "-m",
            "semgrep",
            "--pattern",
            SENTINEL_PATTERN,
            "--lang",
            language,
            "--json",
            repo_path,
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        encoding="utf-8",
    )

    assert semgrep_run.returncode == 0
    try:
        output = json.loads(semgrep_run.stdout)
    except json.JSONDecodeError:
        pytest.fail(
            f"Failed to parse JSON from semgrep output:\n"
            + semgrep_run.stdout
            + semgrep_run.stderr
        )

    if output["errors"]:
        pytest.fail(
            f"Running on {repo_url} with lang {language} had errors: "
            + json.dumps(output["errors"], indent=4)
        )

    if len(output["results"]) != 1 or output["results"][0]["path"] != str(
        sentinel_path
    ):
        pytest.fail(
            f"Running on {repo_url} with lang {language} expected to have one results instead found result: "
            + json.dumps(output["results"], indent=4)
        )


# public_repo_url is a fancy dynamic parameterization defined in conftest.
# In `--qa` mode, it runs every public repo in public_repos.ALL_REPOS
# In regular mode, it runs the first 5 from public_repos.PASSING.
def test_semgrep_on_repo(monkeypatch, clone_github_repo, tmp_path, public_repo_url):
    (tmp_path / "rules").symlink_to(Path(TESTS_PATH / "qa" / "rules").resolve())

    monkeypatch.chdir(tmp_path)

    repo_url = public_repo_url["repo"]
    languages = public_repo_url["languages"]
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

        _assert_sentinel_results(repo_url, repo_path, sentinel_path, language)

    sub_output = subprocess.check_output(
        [
            "python3",
            "-m",
            "semgrep",
            "--config=rules/regex-sentinel.yaml",
            "--strict",
            "--json",
            repo_path,
        ],
        encoding="utf-8",
    )
    output = json.loads(sub_output)

    expected_results_count = len(repo_languages)
    if len(output["results"]) != expected_results_count or len(output["errors"]) != 0:
        pytest.fail(
            f"Running on {repo_url} with regex rules. Expect {expected_results_count} results and no errors but got: "
            + json.dumps(output, indent=4)
        )
