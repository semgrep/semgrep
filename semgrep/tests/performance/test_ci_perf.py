import subprocess
import time

import pytest

from semgrep import util


@pytest.mark.qa
def test_perf(clone_github_repo):
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

    for repo_url, sha, excludes, expected_duration in targets:
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
        assert duration < expected_duration
