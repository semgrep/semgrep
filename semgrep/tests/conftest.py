import json
import subprocess
from pathlib import Path
from typing import List
from typing import Optional
from typing import Sequence
from typing import Union

import pytest


TESTS_PATH = Path(__file__).parent


def _run_semgrep(
    config: Optional[Union[str, Path]] = None,
    *,
    target_name: str = "basic",
    options: Optional[List[Union[str, Path]]] = None,
    output_format: str = "json",
    stderr: bool = False,
) -> str:
    """Run the semgrep CLI.

    :param config: what to pass as --config's value
    :param target_name: which directory within ./e2e/targets/ to scan
    :param options: additional CLI flags to add
    :param output_format: which format to use, valid options are normal, json, and sarif
    :param stderr: whether to merge stderr into the returned string
    """
    if options is None:
        options = []

    options.append("--strict")

    if config is not None:
        options.extend(["--config", config])

    if output_format == "json":
        options.append("--json")
    elif output_format == "sarif":
        options.append("--sarif")

    output = subprocess.check_output(
        ["python3", "-m", "semgrep", *options, Path("targets") / target_name],
        encoding="utf-8",
        stderr=subprocess.STDOUT if stderr else None,
    )

    if output_format in {"json", "sarif"} and not stderr:
        output = json.dumps(json.loads(output), indent=2, sort_keys=True)

    return output


@pytest.fixture
def run_semgrep_in_tmp(monkeypatch, tmp_path):
    monkeypatch.setenv("PYTHONPATH", str(TESTS_PATH.parent.resolve()))

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
