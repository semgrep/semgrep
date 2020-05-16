import subprocess
from pathlib import Path
from typing import NamedTuple

import pytest


class RepoCase(NamedTuple):
    url: str
    sha: str
    language: str

    @property
    def test_id(self):
        return self.url.split("/")[-1]


@pytest.mark.parametrize(
    "repo_case",
    [
        # SHAs of May 6, 2020
        RepoCase("https://github.com/getsentry/sentry", "f25ea5dc", "python"),
        RepoCase("https://github.com/highcharts/highcharts", "04709689", "javascript"),
    ],
    ids=lambda case: case.test_id,
)
def test_public_repos(run_semgrep_in_tmp, benchmark, repo_case):
    subprocess.check_output(["git", "clone", repo_case.url, "repo"])
    subprocess.check_output(
        ["git", "--git-dir", "repo/.git", "checkout", repo_case.sha]
    )
    benchmark(
        subprocess.check_output,
        [
            "python3",
            "-m",
            "semgrep",
            "--jobs",
            "1",
            "--pattern",
            "$X = 156128578192758",
            "--lang",
            repo_case.language,
            "repo",
        ],
    )
