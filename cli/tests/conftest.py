import json
import os
import re
import shlex
import subprocess
import tempfile
from dataclasses import dataclass
from functools import partial
from io import StringIO
from pathlib import Path
from shutil import copytree
from typing import Callable
from typing import Dict
from typing import Iterable
from typing import List
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Union

import colorama
import pytest
from click.testing import CliRunner

from semgrep import __VERSION__
from semgrep.cli import cli
from semgrep.constants import OutputFormat
from semgrep.lsp.server import SemgrepLSPServer

TESTS_PATH = Path(__file__).parent


def mark_masked(obj, path):
    path_items = path.split(".")
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
                mark_masked(o, ".".join(path_items[1:]))


def _clean_stdout(out):
    """Make semgrep's output deterministic."""
    json_output = json.loads(out)
    if json_output.get("version"):
        json_output["version"] = "0.42"

    # Necessary because some tests produce temp files
    if json_output.get("errors"):
        for error in json_output.get("errors"):
            if error.get("spans"):
                for span in error.get("spans"):
                    if span.get("file"):
                        file = span.get("file")
                        span["file"] = file if "tmp" not in file else "tmp/masked/path"

    return json.dumps(json_output)


def _clean_output_json(output_json: str, clean_fingerprint: bool = False) -> str:
    """Make semgrep's output deterministic and nicer to read."""
    try:
        output = json.loads(output_json)
    except json.decoder.JSONDecodeError:
        return output_json

    masked_keys = [
        "tool.driver.semanticVersion",
        "results.extra.metavars.*.unique_id.md5sum",
        "results.*.checks.*.matches",
    ]
    for path in masked_keys:
        mark_masked(output, path)

    # Remove temp file paths
    results = output.get("results")
    if isinstance(results, Sequence):
        # for semgrep scan output
        if output.get("version"):
            output["version"] = "0.42"
        for r in results:
            p = r.get("path")
            if p and "/tmp" in p:
                r["path"] = "/tmp/masked/path"
            if clean_fingerprint:
                r["extra"]["fingerprint"] = "0x42"

    paths = output.get("paths", {})
    if paths.get("scanned"):
        paths["scanned"] = [
            p if "/tmp" not in p else "/tmp/masked/path" for p in paths["scanned"]
        ]
    if paths.get("skipped"):
        paths["skipped"] = [
            {
                **skip,
                "path": skip["path"]
                if "/tmp" not in skip["path"]
                else "/tmp/masked/path",
            }
            for skip in paths["skipped"]
        ]

    # Necessary because some tests produce temp files
    if output.get("errors"):
        for error in output.get("errors"):
            if error.get("spans"):
                for span in error.get("spans"):
                    if span.get("file"):
                        file = span.get("file")
                        span["file"] = file if "tmp" not in file else "tmp/masked/path"

    return json.dumps(output, indent=2, sort_keys=True)


def _clean_output_sarif(output_json: str) -> str:
    try:
        output = json.loads(output_json)
    except json.decoder.JSONDecodeError:
        return output_json

    # Rules are logically a set so the JSON list's order doesn't matter
    # we make the order deterministic here so that snapshots match across runs
    # the proper solution will be https://github.com/joseph-roitman/pytest-snapshot/issues/14
    try:
        output["runs"][0]["tool"]["driver"]["rules"] = sorted(
            output["runs"][0]["tool"]["driver"]["rules"],
            key=lambda rule: str(rule["id"]),
        )
    except (KeyError, IndexError):
        pass

    # Semgrep version is included in sarif output. Verify this independently so
    # snapshot does not need to be updated on version bump
    try:
        assert output["runs"][0]["tool"]["driver"]["semanticVersion"] == __VERSION__
        output["runs"][0]["tool"]["driver"]["semanticVersion"] = "placeholder"
    except (KeyError, IndexError):
        pass

    return json.dumps(output, indent=2, sort_keys=True)


def _clean_output_lsp(out) -> str:
    text = json.dumps(out, indent=2, sort_keys=True)
    # Regex to match anything starting with file://
    capture = re.compile(r"\"file:\/\/[^\s]+\"")

    cleaned = capture.sub('"file:///tmp/masked/path"', text)
    return cleaned


Maskers = Iterable[Union[str, re.Pattern, Callable[[str], str]]]


def mask_capture_group(match: re.Match) -> str:
    if not match.groups():
        return "<MASKED>"
    text: str = match.group()
    for group in match.groups():
        text = text.replace(group, "<MASKED>")
    return text


ALWAYS_MASK: Maskers = (
    _clean_output_json,
    _clean_output_sarif,
    __VERSION__,
    re.compile(r"python (\d+[.]\d+[.]\d+)"),
    re.compile(r'SEMGREP_SETTINGS_FILE="(.+?)"'),
    re.compile(r'SEMGREP_VERSION_CACHE_PATH="(.+?)"'),
)


@dataclass
class SemgrepResult:
    command: str
    raw_stdout: str
    raw_stderr: str
    exit_code: int

    def strip_color(self, text: str) -> str:
        stream = StringIO()
        desaturator = colorama.AnsiToWin32(stream, strip=True)
        desaturator.write(text)
        stream.seek(0)
        return stream.read()

    def mask_text(self, text: str, mask: Optional[Maskers] = None) -> str:
        if mask is None:
            mask = []
        for pattern in [*mask, *ALWAYS_MASK]:
            if isinstance(pattern, str):
                text = text.replace(pattern, "<MASKED>")
            elif isinstance(pattern, re.Pattern):
                text = pattern.sub(mask_capture_group, text)
            elif callable(pattern):
                text = pattern(text)
        return text

    @property
    def stdout(self) -> str:
        return self.mask_text(self.raw_stdout)

    @property
    def stderr(self) -> str:
        return self.mask_text(self.raw_stderr)

    def as_snapshot(self, mask: Optional[Maskers] = None):
        stdout = self.mask_text(self.raw_stdout, mask)
        stderr = self.mask_text(self.raw_stderr, mask)
        sections = {
            "command": self.mask_text(self.command, mask),
            "exit code": self.exit_code,
            "stdout - plain": self.strip_color(stdout),
            "stderr - plain": self.strip_color(stderr),
            "stdout - color": stdout,
            "stderr - color": stderr,
        }
        if (
            sections["stdout - plain"] == sections["stdout - color"]
            and sections["stderr - plain"] == sections["stderr - color"]
        ):
            del sections["stdout - color"]
            del sections["stderr - color"]
        return "\n\n".join(
            f"=== {title}\n{text}\n=== end of {title}"
            for title, text in sections.items()
        )

    def print_debug_info(self) -> None:
        print(
            "=== to reproduce (run with `pytest --pdb` to suspend while temp dirs exist)"
        )
        print(f"$ cd {os.getcwd()}")
        print(f"$ {self.command}")
        print("=== exit code")
        print(self.exit_code)
        print("=== stdout")
        print(self.stdout)
        print("=== stderr")
        print(self.stderr)

    def __iter__(self):
        """For backwards compat with usages like `stdout, stderr = run_semgrep(...)`"""
        yield self.stdout
        yield self.stderr


def _run_semgrep(
    config: Optional[Union[str, Path, List[str]]] = None,
    *,
    target_name: Optional[str] = "basic",
    options: Optional[List[Union[str, Path]]] = None,
    output_format: Optional[OutputFormat] = OutputFormat.JSON,
    strict: bool = True,
    quiet: bool = False,
    env: Optional[Dict[str, str]] = None,
    assert_exit_code: Union[None, int, Set[int]] = 0,
    force_color: Optional[bool] = None,
    assume_targets_dir: bool = True,  # See e2e/test_dependency_aware_rule.py for why this is here
    force_metrics_off: bool = True,
    stdin: Optional[str] = None,
) -> SemgrepResult:
    """Run the semgrep CLI.

    :param config: what to pass as --config's value
    :param target_name: which path (either relative or absolute) within ./e2e/targets/ to scan
    :param options: additional CLI flags to add
    :param output_format: which format to use
    :param stderr: whether to merge stderr into the returned string
    :param settings_file: what setting file for semgrep to use. If None, a random temp file is generated
                          with default params ("has_shown_metrics_notification: true")
    """
    env = {} if not env else env.copy()

    if force_color:
        env["SEMGREP_FORCE_COLOR"] = "true"

    if "SEMGREP_USER_AGENT_APPEND" not in env:
        env["SEMGREP_USER_AGENT_APPEND"] = "pytest"

    # If delete_setting_file is false and a settings file doesnt exist, put a default
    # as we are not testing said setting. Note that if Settings file exists we want to keep it
    # Use a unique settings file so multithreaded pytest works well
    if "SEMGREP_SETTINGS_FILE" not in env:
        unique_settings_file = tempfile.NamedTemporaryFile().name
        Path(unique_settings_file).write_text("has_shown_metrics_notification: true")

        env["SEMGREP_SETTINGS_FILE"] = unique_settings_file
    if "SEMGREP_VERSION_CACHE_PATH" not in env:
        env["SEMGREP_VERSION_CACHE_PATH"] = tempfile.TemporaryDirectory().name
    if "SEMGREP_ENABLE_VERSION_CHECK" not in env:
        env["SEMGREP_ENABLE_VERSION_CHECK"] = "0"
    if force_metrics_off and "SEMGREP_SEND_METRICS" not in env:
        env["SEMGREP_SEND_METRICS"] = "off"

    if options is None:
        options = []

    if strict:
        options.append("--strict")

    if quiet:
        options.append("--quiet")

    if config is not None:
        if isinstance(config, list):
            for conf in config:
                options.extend(["--config", conf])
        else:
            options.extend(["--config", config])

    if output_format == OutputFormat.JSON:
        options.append("--json")
    elif output_format == OutputFormat.GITLAB_SAST:
        options.append("--gitlab-sast")
    elif output_format == OutputFormat.GITLAB_SECRETS:
        options.append("--gitlab-secrets")
    elif output_format == OutputFormat.JUNIT_XML:
        options.append("--junit-xml")
    elif output_format == OutputFormat.SARIF:
        options.append("--sarif")

    targets = []
    if target_name is not None:
        targets.append(
            Path("targets") / target_name if assume_targets_dir else Path(target_name)
        )
    args = " ".join(shlex.quote(str(c)) for c in [*options, *targets])
    env_string = " ".join(f'{k}="{v}"' for k, v in env.items())

    runner = CliRunner(env=env, mix_stderr=False)
    click_result = runner.invoke(cli, args, input=stdin)
    result = SemgrepResult(
        f"{env_string} semgrep {args}",
        click_result.stdout,
        click_result.stderr,
        click_result.exit_code,
    )
    result.print_debug_info()

    if isinstance(assert_exit_code, set):
        assert result.exit_code in assert_exit_code
    elif isinstance(assert_exit_code, int):
        assert result.exit_code == assert_exit_code

    return result


@pytest.fixture
def run_semgrep():
    yield partial(_run_semgrep, strict=False, target_name=None, output_format=None)


@pytest.fixture
def run_semgrep_in_tmp(monkeypatch, tmp_path):
    """
    Note that this can cause failures if Semgrep pollutes either the targets or rules path
    """
    (tmp_path / "targets").symlink_to(Path(TESTS_PATH / "e2e" / "targets").resolve())
    (tmp_path / "rules").symlink_to(Path(TESTS_PATH / "e2e" / "rules").resolve())
    monkeypatch.chdir(tmp_path)

    yield _run_semgrep


@pytest.fixture
def run_semgrep_on_copied_files(monkeypatch, tmp_path):
    """
    Like run_semgrep_in_tmp, but fully copies rule and target data to avoid
    directory pollution, also avoids issues with symlink navigation
    """
    copytree(Path(TESTS_PATH / "e2e" / "targets").resolve(), tmp_path / "targets")
    copytree(Path(TESTS_PATH / "e2e" / "rules").resolve(), tmp_path / "rules")
    monkeypatch.chdir(tmp_path)

    yield _run_semgrep


@pytest.fixture
def git_tmp_path(monkeypatch, tmp_path):
    monkeypatch.chdir(tmp_path)
    # Initialize State
    subprocess.run(["git", "init"], check=True, capture_output=True)
    subprocess.run(
        ["git", "config", "user.email", "baselinetest@r2c.dev"],
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "config", "user.name", "Baseline Test"],
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "checkout", "-B", "main"],
        check=True,
        capture_output=True,
    )
    yield tmp_path


@pytest.fixture
def lsp(monkeypatch, tmp_path):
    """Return an initialized lsp"""

    (tmp_path / "targets").symlink_to(Path(TESTS_PATH / "e2e" / "targets").resolve())
    (tmp_path / "rules").symlink_to(Path(TESTS_PATH / "e2e" / "rules").resolve())
    ls = SemgrepLSPServer(StringIO, StringIO)
    monkeypatch.chdir(tmp_path)
    return ls
