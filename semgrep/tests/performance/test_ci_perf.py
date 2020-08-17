import subprocess
import time

import pytest


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

    # Dvna takes about 30 sec
    target_path = clone_github_repo(
        repo_url="https://github.com/appsecco/dvna", sha="c637437"
    )
    start = time.time()
    subprocess.check_output(
        ["python3", "-m" "semgrep", "--config", str(rules_path), str(target_path)]
    )
    duration = time.time() - start
    print(duration)
    assert duration < 40

    # Running on Juice Shop without three.js takes ~150 sec on 2019MBP 15"
    # takes ~270 on GHA
    target_path = clone_github_repo(
        repo_url="https://github.com/bkimminich/juice-shop",
        sha="98633f5ef242bf943608324a562058b22eca6dfe",
    )
    start = time.time()
    subprocess.check_output(
        [
            "python3",
            "-m" "semgrep",
            "--config",
            str(rules_path / "njsscan/rules/semantic_grep"),
            "--exclude",
            "three.js",
            str(target_path),
        ]
    )
    duration = time.time() - start
    print(duration)
    assert duration < 300
