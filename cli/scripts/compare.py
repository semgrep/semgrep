#!/usr/bin/env python3
import os
import subprocess
import tempfile
from typing import Sequence

import click
import requests
from ruamel import yaml

from semgrep.semgrep_types import LANGUAGE  # type: ignore  ## mypy isn't picking up src/ in this context


SEMGREP_DEV_TIMEOUT_S = 30.0


@click.command()
@click.argument("start", type=str)
@click.argument(
    "end",
    type=str,
)
@click.argument("snippet", type=str)
def compare(start: str, end: str, snippet: str) -> int:
    """
    Compares behavior of two versions of Semgrep on a rule ID

    START - The first version of Semgrep to run

    END - The second version of Semgrep to run

    SNIPPET - A snippet or rule ID (e.g. "Wlz")
    """

    url = f"https://semgrep.dev/api/registry/rule/{snippet}?definition=1&test_cases=1"
    data = requests.get(url, timeout=SEMGREP_DEV_TIMEOUT_S).json()

    definition = data["definition"]
    test_case = data["test_cases"][0]
    language = test_case["language"]
    target = test_case["target"]

    def docker_cmd(dir: str, version: str) -> Sequence[str]:
        return [
            "docker",
            "run",
            "-v",
            f"{dir}:/src",
            f"returntocorp/semgrep:{version}",
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

        target_name = f"target.{LANGUAGE.definition_by_id[language].exts[0]}"
        with open(target_name, "w") as fd:
            fd.write(target)

        click.secho(f"===== RUNNING WITH VERSION {start} =====\n", fg="blue", bold=True)
        # The following rule shouldn't be running on Semgrep, it's an FP factory
        # nosemgrep: python.lang.security.audit.dangerous-subprocess-use.dangerous-subprocess-use
        subprocess.run(docker_cmd(td, start))
        click.secho(
            f"\n\n===== RUNNING WITH VERSION {end} =====\n", fg="blue", bold=True
        )
        # nosemgrep: python.lang.security.audit.dangerous-subprocess-use.dangerous-subprocess-use
        subprocess.run(docker_cmd(td, end))

    return 0


if __name__ == "__main__":
    compare()
