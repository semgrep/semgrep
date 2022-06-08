import json
import os
import shlex
import tempfile
from pathlib import Path
from shutil import copytree
from typing import Callable
from typing import Dict
from typing import List
from typing import Mapping
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Tuple
from typing import Union

import pytest
from click.testing import CliRunner

from semgrep import __VERSION__
from semgrep.cli import cli
from semgrep.constants import OutputFormat

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


def _clean_output_json(output_json: str) -> str:
    """Make semgrep's output deterministic and nicer to read."""
    output = json.loads(output_json)

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

    return json.dumps(output, indent=2, sort_keys=True)


def _clean_output_sarif(output):
    # Rules are logically a set so the JSON list's order doesn't matter
    # we make the order deterministic here so that snapshots match across runs
    # the proper solution will be https://github.com/joseph-roitman/pytest-snapshot/issues/14
    output["runs"][0]["tool"]["driver"]["rules"] = sorted(
        output["runs"][0]["tool"]["driver"]["rules"],
        key=lambda rule: str(rule["id"]),
    )

    # Semgrep version is included in sarif output. Verify this independently so
    # snapshot does not need to be updated on version bump
    assert output["runs"][0]["tool"]["driver"]["semanticVersion"] == __VERSION__
    output["runs"][0]["tool"]["driver"]["semanticVersion"] = "placeholder"

    return output


CLEANERS: Mapping[str, Callable[[str], str]] = {
    "--sarif": lambda s: json.dumps(_clean_output_sarif(json.loads(s))),
    "--gitlab-sast": _clean_output_json,
    "--gitlab-secrets": _clean_output_json,
    "--json": _clean_output_json,
}


def _run_semgrep(
    config: Optional[Union[str, Path, List[str]]] = None,
    *,
    target_name: str = "basic",
    options: Optional[List[Union[str, Path]]] = None,
    output_format: OutputFormat = OutputFormat.JSON,
    strict: bool = True,
    quiet: bool = False,
    env: Optional[Dict[str, str]] = None,
    assert_exit_code: Union[None, int, Set[int]] = 0,
    settings_file: Optional[str] = None,
    force_color: Optional[bool] = None,
    assume_targets_dir: bool = True,  # See e2e/test_dependency_aware_rule.py for why this is here
    force_metrics_off: bool = True,
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
    env = {} if not env else env.copy()

    if force_color:
        env["SEMGREP_FORCE_COLOR"] = "true"

    if "SEMGREP_USER_AGENT_APPEND" not in env:
        env["SEMGREP_USER_AGENT_APPEND"] = "pytest"

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
    if force_metrics_off:
        options.append("--metrics=off")

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

    target = Path("targets") / target_name if assume_targets_dir else Path(target_name)
    args = " ".join(shlex.quote(str(c)) for c in [*options, target])

    runner = CliRunner(env=env, mix_stderr=False)
    result = runner.invoke(cli, args)

    # some helpful output for reproducing issues
    env_string = " ".join(f'{k}="{v}"' for k, v in env.items())
    print(f"### COMMANDS")
    print(f"$ cd {os.getcwd()}")
    print(f"$ {env_string} semgrep {args}")
    print("### STDOUT")
    print(result.stdout)
    print("### STDERR")
    print(result.stderr)
    print("### END\n")

    if isinstance(assert_exit_code, set):
        assert result.exit_code in assert_exit_code
    elif isinstance(assert_exit_code, int):
        assert result.exit_code == assert_exit_code

    stdout = (
        _clean_output_json(result.stdout)
        if result.stdout and output_format.is_json()
        else result.stdout
    )

    return stdout, result.stderr


@pytest.fixture
def run_semgrep_in_tmp(monkeypatch, tmp_path):
    (tmp_path / "targets").symlink_to(Path(TESTS_PATH / "e2e" / "targets").resolve())
    (tmp_path / "rules").symlink_to(Path(TESTS_PATH / "e2e" / "rules").resolve())
    monkeypatch.chdir(tmp_path)

    yield _run_semgrep


# Needed to test the project-depends-on rules
# pathlib.glob (and semgrep by extension) do not traverse into symlinks
# Lockfile targeting begins at the parent of the first semgrep target
# which in this case is tmp_path, which normally contains only symlinks :/
@pytest.fixture
def run_semgrep_in_tmp_no_symlink(monkeypatch, tmp_path):
    copytree(Path(TESTS_PATH / "e2e" / "targets").resolve(), tmp_path / "targets")
    copytree(Path(TESTS_PATH / "e2e" / "rules").resolve(), tmp_path / "rules")
    monkeypatch.chdir(tmp_path)

    yield _run_semgrep
