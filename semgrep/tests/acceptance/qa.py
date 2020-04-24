import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from contextlib import contextmanager
from pathlib import Path
from typing import Any
from typing import Callable
from typing import Iterator
from typing import List
from typing import Mapping
from typing import Optional

import yaml


@contextmanager
def git_checkout(url: str, commit_hash: str) -> Iterator[str]:
    """
        Clones URL into destination and checks out commit_hash

        Returns name of directory url was cloned into
    """
    with tempfile.TemporaryDirectory() as destination:
        print("starting clone")
        subprocess.run(
            ["git", "clone", url, destination],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=True,
        )
        print("done clone")
        subprocess.run(
            ["git", "checkout", commit_hash],
            cwd=destination,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=True,
        )
        subprocess.run(
            ["git", "clean", "-xdf"],
            cwd=destination,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=True,
        )
        yield destination


def run_repo(target: str, rewrite: bool = False) -> None:
    """
    Runs semgrep on github repo at commithash in target/config.yaml
    Runs rules in target/rule.yaml

    Assert output is the same as target/expected
    """
    with open(f"tests/acceptance/{target}/config.yaml") as file:
        info = yaml.safe_load(file)
        print(info)

    target_repo = info["repo_url"]
    target_hash = info["commit_hash"]
    expected_output_file = f"tests/acceptance/{target}/expected"
    actual_output_file = f"tests/acceptance/{target}/actual"

    with git_checkout(target_repo, target_hash) as target_dir:
        config_dir = Path(f"tests/acceptance/{target}/rule.yaml")

        command = ["semgrep", f"--config={config_dir.resolve()}", "--json", "."]

        runned = subprocess.run(
            command,
            cwd=target_dir,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            encoding="utf-8",
        )

        actual_json = json.loads(runned.stdout)

        if rewrite:
            with open(expected_output_file, "w") as file:
                json.dump(actual_json, file, sort_keys=True, indent=4)

        with open(actual_output_file, "w") as file:
            json.dump(actual_json, file, sort_keys=True, indent=4)

        diff_run = subprocess.run(
            ["diff", actual_output_file, expected_output_file],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            encoding="utf-8",
        )
        os.remove(actual_output_file)
        if diff_run.returncode != 0:
            print(diff_run.stdout)
        assert diff_run.returncode == 0


TARGETS = ["zulip"]


def test_repos() -> None:
    for target in TARGETS:
        run_repo(target)


if __name__ == "__main__":
    for t in TARGETS:
        run_repo(t, rewrite=True)
