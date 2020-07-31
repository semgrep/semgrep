import json
import subprocess
from pathlib import Path

import pytest

from ..conftest import TESTS_PATH

SENTINEL_VALUE = 87518275812375164


# public_repo_url is a fancy dynamic parameterization defined in conftest.
# In `--qa` mode, it runs every public repo in public_repos.ALL_REPOS
# In regular mode, it runs the first 5 from public_repos.PASSING.
def test_semgrep_on_repo(monkeypatch, clone_github_repo, tmp_path, public_repo_url):
    (tmp_path / "rules").symlink_to(Path(TESTS_PATH / "qa" / "rules").resolve())
    repo_url = public_repo_url

    monkeypatch.chdir(tmp_path)

    repo_path = clone_github_repo(repo_url=public_repo_url)

    languages = {
        "python": "py",
        "go": "go",
        "javascript": "js",
    }
    for language, file_ext in languages.items():
        sentinel_path = repo_path / f"sentinel.{file_ext}"
        with sentinel_path.open("w") as sentinel_file:
            if language == "go":
                sentinel_file.write(f"package Foo\nconst x = {SENTINEL_VALUE}")
            else:
                sentinel_file.write(f"x = {SENTINEL_VALUE}")

        semgrep_run = subprocess.run(
            [
                "python3",
                "-m",
                "semgrep",
                "--pattern",
                f"$X = {SENTINEL_VALUE}",
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

        if output["errors"] != []:
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

    output = subprocess.check_output(
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
    output = json.loads(output)

    if len(output["results"]) != 3 or len(output["errors"]) != 0:
        pytest.fail(
            f"Running on {repo_url} with regex rules. Expect 3 results and no errors but got: "
            + json.dumps(output, indent=4)
        )
