#!/usr/bin/env python3
# Compares behavior of two versions of Semgrep on a rule ID. See the Python
# docstring below for compare() for more information.
import os
import subprocess
import sys
import tempfile
from typing import Sequence

import click
import requests
from ruamel import yaml

from semgrep.semgrep_types import LANGUAGE


SEMGREP_DEV_TIMEOUT_S = 30.0


def compute_docker_executable(use_podman: bool) -> str:
    if use_podman:
        return "podman"

    return "docker"


@click.command()
@click.argument("start", type=str)
@click.argument(
    "end",
    type=str,
)
@click.argument("snippet", type=str)
@click.option("--use-podman", is_flag=True, help="Use podman instead of docker.")
def compare(start: str, end: str, snippet: str, use_podman: bool) -> int:
    """
    Compares behavior of two versions of Semgrep on a rule ID

    START - The first version of Semgrep to run

    END - The second version of Semgrep to run

    SNIPPET - A snippet or rule ID (e.g. "Wlz" or "ievans:print-to-logger2")
    """

    is_ruleset = ":" in snippet
    collection_path = "rulesets" if is_ruleset else "rules"
    url = f"https://semgrep.dev/api/registry/{collection_path}/{snippet}?definition=1&test_cases=1"
    headers = {"Accept": "application/json"}
    # nosemgrep: python.flask.security.injection.ssrf-requests.ssrf-requests
    data = requests.get(url, timeout=SEMGREP_DEV_TIMEOUT_S, headers=headers).json()
    data = data["rules"][0] if is_ruleset else data

    definition = data["definition"]
    test_case = data["test_cases"][0]
    language = test_case["language"]
    target = test_case["target"]

    def docker_cmd(use_podman: bool, dir: str, version: str) -> Sequence[str]:
        docker_executable = compute_docker_executable(use_podman)
        return [
            docker_executable,
            "run",
            "--rm",
            *(["-ti"] if sys.stdout.isatty() else []),
            *(["--security-opt", "label=disable"] if use_podman else []),
            "-v",
            f"{dir}:/src",
            f"semgrep/semgrep:{version}",
            "semgrep",
            "scan",
            "--config",
            "semgrep.yml",
            ".",
        ]

    with tempfile.TemporaryDirectory() as td:
        os.chdir(td)

        with open("semgrep.yml", "w") as fd:
            yaml.safe_dump(definition, fd)  # type: ignore ## for some reason this is missing from ruamel stub

        target_name = (
            f"target{next(e for e in LANGUAGE.definition_by_id[language].exts)}"
        )
        with open(target_name, "w") as fd:
            fd.write(target)

        click.secho(f"===== RUNNING WITH VERSION {start} =====\n", fg="blue", bold=True)
        # The following rule shouldn't be running on Semgrep, it's an FP factory
        # nosemgrep: python.lang.security.audit.dangerous-subprocess-use.dangerous-subprocess-use
        subprocess.run(docker_cmd(use_podman, td, start))
        click.secho(
            f"\n\n===== RUNNING WITH VERSION {end} =====\n", fg="blue", bold=True
        )
        # nosemgrep: python.lang.security.audit.dangerous-subprocess-use.dangerous-subprocess-use
        subprocess.run(docker_cmd(use_podman, td, end))

    return 0


if __name__ == "__main__":
    compare()
