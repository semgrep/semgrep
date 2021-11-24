import contextlib
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Sequence
from typing import Tuple
from typing import Union

import pytest

from semgrep.constants import OutputFormat

TESTS_PATH = Path(__file__).parent

MASKED_KEYS = [
    "tool.driver.semanticVersion",
    "results.extra.metavars.*.unique_id.md5sum",
    "results.*.checks.*.matches",
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
            if isinstance(o, dict):
                _mark_masked(o, path_items[1:])


def _clean_output_json(output_json: str) -> str:
    """Make semgrep's output deterministic and nicer to read."""
    output = json.loads(output_json)
    for path in MASKED_KEYS:
        mark_masked(output, path)

    # Remove temp file paths
    results = output.get("results")
    if isinstance(results, Sequence):
        for r in results:
            p = r.get("path")
            if p and "/tmp" in p:
                del r["path"]

    return json.dumps(output, indent=2, sort_keys=True)


def _run_semgrep(
    config: Optional[Union[str, Path, List[str]]] = None,
    *,
    target_name: str = "basic",
    options: Optional[List[Union[str, Path]]] = None,
    output_format: OutputFormat = OutputFormat.JSON,
    strict: bool = True,
    quiet: bool = False,
    env: Optional[Dict[str, str]] = None,
    fail_on_nonzero: bool = True,
    settings_file: Optional[str] = None,
) -> Tuple[str, str]:
    """Run the semgrep CLI.

    :param config: what to pass as --config's value
    :param target_name: which directory within ./e2e/targets/ to scan
    :param options: additional CLI flags to add
    :param output_format: which format to use
    :param stderr: whether to merge stderr into the returned string
    :param settings_file: what setting file for semgrep to use. If None, a random temp file is generated
                          with default params ("has_shown_metrics_notification: true")
    """

    # If delete_setting_file is false and a settings file doesnt exist, put a default
    # as we are not testing said setting. Note that if Settings file exists we want to keep it
    # Use a unique settings file so multithreaded pytest works well

    if not env:
        env = {}

    if "SEMGREP_USER_AGENT_APPEND" not in env:
        env["SEMGREP_USER_AGENT_APPEND"] = "testing"

    if not settings_file:
        unique_settings_file = tempfile.NamedTemporaryFile().name
        Path(unique_settings_file).write_text("has_shown_metrics_notification: true")

        env["SEMGREP_SETTINGS_FILE"] = unique_settings_file
    else:
        env["SEMGREP_SETTINGS_FILE"] = settings_file

    if options is None:
        options = []

    if strict:
        options.append("--strict")

    if quiet:
        options.append("--quiet")

    options.append("--disable-version-check")

    if config is not None:
        if isinstance(config, list):
            for conf in config:
                options.extend(["--config", conf])
        else:
            options.extend(["--config", config])

    if output_format == OutputFormat.JSON:
        options.append("--json")
    elif output_format == OutputFormat.JUNIT_XML:
        options.append("--junit-xml")
    elif output_format == OutputFormat.SARIF:
        options.append("--sarif")

    cmd = [sys.executable, "-m", "semgrep", *options, Path("targets") / target_name]
    # join here so that one can easily copy-paste the command
    str_cmd = " ".join(str(c) for c in cmd)
    print(f"current directory: {os.getcwd()}")
    print(f"semgrep command: {str_cmd}")
    output = subprocess.run(
        cmd,
        encoding="utf-8",
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
        # LANG is necessary to work with Python 3.6
        env={**env, "LANG": "en_US.UTF-8"},
    )

    if fail_on_nonzero and output.returncode > 0:
        print("--- stdout from semgrep process ---")
        print(output.stdout)
        print("--- end semgrep stdout ---")
        print("--- stderr from semgrep process ---")
        print(output.stderr)
        print("--- end semgrep stderr ---")
        raise subprocess.CalledProcessError(
            returncode=output.returncode,
            cmd=str_cmd,
            output=output.stdout,
            stderr=output.stderr,
        )

    stdout = (
        _clean_output_json(output.stdout)
        if output.stdout and output_format.is_json()
        else output.stdout
    )

    return stdout, output.stderr


@contextlib.contextmanager
def chdir(dirname=None):
    curdir = os.getcwd()
    try:
        if dirname is not None:
            os.chdir(dirname)
        yield
    finally:
        os.chdir(curdir)


@pytest.fixture
def run_semgrep_in_tmp(monkeypatch, tmp_path):
    (tmp_path / "targets").symlink_to(Path(TESTS_PATH / "e2e" / "targets").resolve())
    (tmp_path / "rules").symlink_to(Path(TESTS_PATH / "e2e" / "rules").resolve())
    monkeypatch.chdir(tmp_path)

    yield _run_semgrep
