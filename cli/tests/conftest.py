# Define so-called pytest fixtures for our tests.
#
# Read about pytest's fixtures if you want to understand where test
# function parameters come from. This is the introduction at
# https://docs.pytest.org/en/latest/how-to/fixtures.html :
#
#   At a basic level, test functions request fixtures they require by
#   declaring them as arguments.
#
#   When pytest goes to run a test, it looks at the parameters in that
#   test function’s signature, and then searches for fixtures that have
#   the same names as those parameters. Once pytest finds them, it runs
#   those fixtures, captures what they returned (if anything), and passes
#   those objects into the test function as arguments.
#
##############################################################################
# Prelude
##############################################################################
# Helper functions and classes useful for writing tests.
import contextlib
import json
import os
import re
import shlex
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from functools import partial
from io import StringIO
from pathlib import Path
from shutil import copytree
from typing import Callable
from typing import ContextManager
from typing import Dict
from typing import Iterable
from typing import List
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Union

import colorama
import pytest
from ruamel.yaml import YAML
from tests import fixtures
from tests.semgrep_runner import SemgrepRunner
from tests.semgrep_runner import USE_OSEMGREP

from semgrep import __VERSION__
from semgrep.cli import cli
from semgrep.constants import OutputFormat

##############################################################################
# Constants
##############################################################################

TESTS_PATH = Path(__file__).parent
RULES_PATH = Path(TESTS_PATH / "default" / "e2e" / "rules")
TARGETS_PATH = Path(TESTS_PATH / "default" / "e2e" / "targets")

##############################################################################
# Pytest hacks
##############################################################################


# ???
def pytest_addoption(parser: pytest.Parser) -> None:
    parser.addoption(
        "--run-only-snapshots",
        action="store_true",
        default=False,
        help="Filter test execution to tests that use pytest-snapshot",
    )

    parser.addoption(
        "--run-lockfileless",
        action="store_true",
        default=False,
        help="Include tests marked as requiring lockfileless environment dependencies.",
    )


# ???
def pytest_collection_modifyitems(
    items: List[pytest.Item], config: pytest.Config
) -> None:
    if config.getoption("--run-only-snapshots"):
        selected_items: List[pytest.Item] = []
        deselected_items: List[pytest.Item] = []

        for item in items:
            group = (
                selected_items
                if "snapshot" in getattr(item, "fixturenames", ())
                else deselected_items
            )
            group.append(item)

        config.hook.pytest_deselected(items=deselected_items)
        items[:] = selected_items

    skip_lockfileless = pytest.mark.skip(reason="need --run-lockfileless to run")
    if not config.getoption("--run-lockfileless"):
        for item in items:
            if "requires_lockfileless_deps" in item.keywords:
                item.add_marker(skip_lockfileless)


##############################################################################
# Helper functions/classes
##############################################################################


class str_containing:
    """Assert that a given string meets some expectations."""

    def __init__(self, pattern, flags=0):
        self._pattern = pattern

    def __eq__(self, actual):
        return self._pattern in actual

    def __repr__(self):
        return self._pattern


def make_semgrepconfig_file(dir_path: Path, contents: str) -> None:
    semgrepconfig_path = dir_path / ".semgrepconfig"
    semgrepconfig_path.write_text(contents)


def make_settings_file(unique_path: Path) -> None:
    Path(unique_path).write_text(
        "anonymous_user_id: 5f52484c-3f82-4779-9353-b29bbd3193b6\n"
        "has_shown_metrics_notification: true\n"
    )


def load_anonymous_user_id(settings_file: Path) -> Optional[str]:
    with open(settings_file) as fd:
        yaml_contents = YAML(typ="safe").load(fd)
    raw_value = yaml_contents.get("anonymous_user_id")
    return f"{raw_value}" if raw_value else None


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

    return json.dumps(json_output)


def _clean_output_if_json(output_json: str, clean_fingerprint: bool) -> str:
    """Make semgrep's output deterministic and nicer to read."""
    try:
        output = json.loads(output_json)
    except json.decoder.JSONDecodeError:
        return output_json

    masked_keys = [
        "tool.driver.semanticVersion",
        "results.*.checks.*.matches",
    ]
    for path in masked_keys:
        mark_masked(output, path)

    # The masking code below is a little complicated. We could use the
    # regexp-based mechanism above (mark_masked) for everything to simplify
    # the porting to osemgrep.

    # Remove temp file paths
    results = output.get("results")
    if isinstance(results, Sequence):
        # for semgrep scan output
        if output.get("version"):
            output["version"] = "0.42"
        for r in results:
            p = r.get("path")
            if p and tempfile.gettempdir() in p:
                r["path"] = "/tmp/masked/path"
            if clean_fingerprint:
                r["extra"]["fingerprint"] = "0x42"

    return json.dumps(output, indent=2, sort_keys=True)


Maskers = Iterable[Union[str, re.Pattern, Callable[[str], str]]]


def mask_capture_group(match: re.Match) -> str:
    if not match.groups():
        return "<MASKED>"
    text: str = match.group()
    for group in match.groups():
        text = text.replace(group, "<MASKED>") if group else text
    return text


def mask_times(result_json: str) -> str:
    result = json.loads(result_json)

    def zero_times(value):
        if type(value) == float:
            return 2.022
        elif type(value) == list:
            return [zero_times(val) for val in value]
        elif type(value) == dict:
            return {k: zero_times(v) for k, v in value.items()}
        else:
            return value

    if "time" in result:
        result["time"] = zero_times(result["time"])
    return json.dumps(result, indent=2, sort_keys=True)


FLOATS = re.compile("([0-9]+).([0-9]+)")


def mask_floats(text_output: str) -> str:
    return re.sub(FLOATS, "x.xxx", text_output)


# ProTip: make sure your regexps can't match JSON quotes so as to keep any
# JSON parseable after a substitution!
ALWAYS_MASK: Maskers = (
    __VERSION__,
    re.compile(r"python (\d+[.]\d+[.]\d+[ ]+)"),
    re.compile(r'SEMGREP_SETTINGS_FILE="(.+?)"'),
    re.compile(r'SEMGREP_VERSION_CACHE_PATH="(.+?)"'),
    # Dates
    re.compile(r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:(?:\.\d+)?Z)?"),
    # Hide any substring that resembles a temporary file path.
    # This may be a little too broad but it's simpler than inspecting
    # specific JSON fields on case-per-case basis.
    #
    # In the future, we may have to hide the temporary folder since it
    # can vary from one OS to another.
    # This regexp masks the tail of a path containing 'tmp' or '/tmp'.
    re.compile(f"((?:{tempfile.gettempdir()})(?:/[A-Za-z0-9_.-]*)*)"),
    # osemgrep only. Needed to match the pysemgrep output b/c pysemgrep
    # uses a temporary path to store rules by osemgrep doesn't.
    re.compile(r'"path": *"(rules/[^"]*)"'),
)


def mask_variable_text(
    text: str, mask: Optional[Maskers] = None, clean_fingerprint: bool = True
) -> str:
    if mask is None:
        mask = []
    for pattern in [*mask, *ALWAYS_MASK]:
        if isinstance(pattern, str):
            text = text.replace(pattern, "<MASKED>")
        elif isinstance(pattern, re.Pattern):
            text = pattern.sub(mask_capture_group, text)
        elif callable(pattern):
            text = pattern(text)
    # strip trailing whitespace characters that are emitted by pysemgrep,
    # but we do not plan to emit them in osemgrep
    text = re.sub(r"[ \t]+$", "", text, flags=re.M)
    # special code for JSON cleaning, used to be in ALWAYS_MASK
    # but sometimes we want fingerprint masking and sometimes not
    text = _clean_output_if_json(text, clean_fingerprint)
    return text


# GIT_CONFIG_NOGLOBAL=true prevents reading the user's git configuration
# which varies from one developer to another and causes variable output.
def create_git_repo() -> None:
    os.system("GIT_CONFIG_NOGLOBAL=true git init")
    os.system("GIT_CONFIG_NOGLOBAL=true git add .")
    os.system("GIT_CONFIG_NOGLOBAL=true git commit -m 'add files'")


##############################################################################
# Calls to semgrep
##############################################################################


@dataclass
class SemgrepResult:
    command: str
    raw_stdout: str
    raw_stderr: str
    exit_code: int
    clean_fingerprint: bool

    def strip_color(self, text: str) -> str:
        stream = StringIO()
        desaturator = colorama.AnsiToWin32(stream, strip=True)
        desaturator.write(text)
        stream.seek(0)
        return stream.read()

    @property
    def stdout(self) -> str:
        return mask_variable_text(
            self.raw_stdout, clean_fingerprint=self.clean_fingerprint
        )

    @property
    def stderr(self) -> str:
        return mask_variable_text(
            self.raw_stderr, clean_fingerprint=self.clean_fingerprint
        )

    def as_snapshot(self, mask: Optional[Maskers] = None):
        stdout = mask_variable_text(
            self.raw_stdout, mask, clean_fingerprint=self.clean_fingerprint
        )
        stderr = mask_variable_text(
            self.raw_stderr, mask, clean_fingerprint=self.clean_fingerprint
        )
        # This is a list of pairs (title, data) containing different
        # kinds of output to put into the snapshot.
        sections = {
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
            # Minimize duplicate output.
            sections["stdout - color"] = "<same as above: stdout - plain>"
            sections["stderr - color"] = "<same as above: stderr - plain>"
        return "\n\n".join(
            f"=== {title}\n{text}\n=== end of {title}"
            for title, text in sections.items()
        )

    def print_debug_info(self) -> None:
        print(
            "=== to reproduce (run with `pytest --pdb` to suspend while temp dirs exist)",
            file=sys.stderr,
        )
        print(f"$ cd {os.getcwd()}", file=sys.stderr)
        print(f"$ {self.command}", file=sys.stderr)
        print("=== exit code", file=sys.stderr)
        print(self.exit_code, file=sys.stderr)
        print("=== stdout", file=sys.stderr)
        print(self.stdout, file=sys.stderr)
        print("=== stderr", file=sys.stderr)
        print(self.stderr, file=sys.stderr)

    def __iter__(self):
        """For backwards compat with usages like `stdout, stderr = run_semgrep(...)`"""
        yield self.stdout
        yield self.stderr


# Implements the 'RunSemgrep' function type (type checking is done
# right after this definition) defined in 'fixtures.py'
# coupling: if you add params, you'll need to also modify fixtures.py
def _run_semgrep(
    config: Optional[Union[str, Path, List[str]]] = None,
    *,
    target_name: Optional[str] = None,
    subcommand: Optional[str] = None,
    options: Optional[List[str]] = None,
    output_format: Optional[OutputFormat] = None,
    strict: bool = False,
    quiet: bool = False,
    env: Optional[Dict[str, str]] = None,
    assert_exit_code: Union[None, int, Set[int]] = 0,
    force_color: Optional[bool] = None,
    # See e2e/test_dependency_aware_rule.py for why this is here
    assume_targets_dir: bool = True,
    force_metrics_off: bool = True,
    stdin: Optional[str] = None,
    clean_fingerprint: bool = True,
    # Deprecated! see semgrep_runner.py toplevel comment
    use_click_runner: bool = False,
    prepare_workspace: Callable[[], None] = lambda: None,
    teardown_workspace: Callable[[], None] = lambda: None,
    context_manager: Optional[ContextManager] = None,
    is_logged_in_weak=False,
    osemgrep_force_project_root: Optional[str] = None,
) -> SemgrepResult:
    """Run the semgrep CLI.

    :param config: what to pass as --config's value
    :param target_name: which path (either relative or absolute) within ./default/e2e/targets/ to scan
    :param options: additional CLI flags to add
    :param output_format: which format to use
    :param stderr: whether to merge stderr into the returned string
    :param settings_file: what setting file for semgrep to use. If None, a random temp file is generated
                          with default params for anonymous_user_id and has_shown_metrics_notification
    """
    try:
        prepare_workspace()

        with context_manager or contextlib.nullcontext():
            env = {} if not env else env.copy()

            if force_color:
                env["SEMGREP_FORCE_COLOR"] = "true"
                # NOTE: We should also apply the known color flags to the env
                env["FORCE_COLOR"] = "1"
                if "NO_COLOR" in env:
                    del env["NO_COLOR"]

            if "SEMGREP_USER_AGENT_APPEND" not in env:
                env["SEMGREP_USER_AGENT_APPEND"] = "pytest"

            # If delete_setting_file is false and a settings file doesnt exist, put a default
            # as we are not testing said setting. Note that if Settings file exists we want to keep it
            # Use a unique settings file so multithreaded pytest works well
            if "SEMGREP_SETTINGS_FILE" not in env:
                unique_settings_file = tempfile.NamedTemporaryFile().name
                make_settings_file(Path(unique_settings_file))
                env["SEMGREP_SETTINGS_FILE"] = unique_settings_file
            if "SEMGREP_VERSION_CACHE_PATH" not in env:
                env["SEMGREP_VERSION_CACHE_PATH"] = tempfile.TemporaryDirectory().name
            if "SEMGREP_ENABLE_VERSION_CHECK" not in env:
                env["SEMGREP_ENABLE_VERSION_CHECK"] = "0"
            if force_metrics_off and "SEMGREP_SEND_METRICS" not in env:
                env["SEMGREP_SEND_METRICS"] = "off"

            # In https://github.com/semgrep/semgrep-proprietary/pull/2605
            # we started to gate some JSON fields with an is_logged_in check
            # and certain tests needs those JSON fields hence this parameter
            if is_logged_in_weak and "SEMGREP_APP_TOKEN" not in env:
                env["SEMGREP_APP_TOKEN"] = "fake_token"

            if options is None:
                options = []

            # This is a hack to make osemgrep's new semgrepignore behavior
            # compatible with pysemgrep when the current folder is not
            # the project's root.
            # - pysemgrep will use the .semgrepignore in the current folder
            # - osemgrep will locate the project root and use all the
            #   .semgrepignore and .gitignore files it finds in the project.
            # In tests, we want to ignore the project-wide's semgrepignore.
            # This is what the '--project-root .' option achieves.
            if (
                (subcommand is None or subcommand == "scan")
                and USE_OSEMGREP
                and osemgrep_force_project_root
            ):
                options.extend(["--project-root", osemgrep_force_project_root])

            if strict:
                options.append("--strict")

            if quiet:
                options.append("--quiet")

            if config is not None:
                if isinstance(config, list):
                    for conf in config:
                        options.extend(["--config", conf])
                else:
                    options.extend(["--config", str(config)])

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
                    Path("targets") / target_name
                    if assume_targets_dir
                    else Path(target_name)
                )
            args = " ".join(shlex.quote(str(c)) for c in [*options, *targets])
            env_string = " ".join(f'{k}="{v}"' for k, v in env.items())

            runner = SemgrepRunner(
                env=env, mix_stderr=False, use_click_runner=use_click_runner
            )
            click_result = runner.invoke(
                cli, subcommand=subcommand, args=args, input=stdin
            )
            subcommand_prefix = f"{subcommand} " if subcommand else ""
            result = SemgrepResult(
                # the actual executable was either semgrep or osemgrep. Is it bad?
                f"{env_string} semgrep {subcommand_prefix}{args}",
                click_result.stdout,
                click_result.stderr,
                click_result.exit_code,
                clean_fingerprint,
            )
            result.print_debug_info()

            if isinstance(assert_exit_code, set):
                assert result.exit_code in assert_exit_code
            elif isinstance(assert_exit_code, int):
                assert result.exit_code == assert_exit_code

            return result

    finally:
        teardown_workspace()


##############################################################################
# Fixtures
##############################################################################
#
# Warning to naive programmers:
#
#   # here's some utility function that we want to make available to tests:
#   @pytest.fixture
#   def foo():
#       print("hello")
#
#   # here's a test in some other test file:
#   def test_whatever(foo):
#       foo()
#
# causes pytest to call test_whatever(foo) for us! If you're paying
# attention, you'll notice that the following is not equivalent and won't
# work because we didn't define a 'bar' fixture:
#
#   def test_whatever(bar):
#       bar()
#


@pytest.fixture
def run_semgrep() -> fixtures.RunSemgrep:
    return _run_semgrep


@pytest.fixture()
def unique_home_dir(monkeypatch: pytest.MonkeyPatch, tmp_path: Path):
    """
    Assign the home directory to a unique temporary directory.
    """
    monkeypatch.setattr(Path, "home", lambda: tmp_path)
    yield tmp_path


# Provide a run_semgrep function with alternate defaults
_run_strict_semgrep_on_basic_targets_with_json_output: fixtures.RunSemgrep = partial(
    _run_semgrep,
    strict=True,
    target_name="basic",
    output_format=OutputFormat.JSON,
    # In the setup we use, 'targets' is a symlink in a temporary folder.
    # It's incompatible with the project root being '.' because
    # the real path of the project root must be a prefix of the real path
    # of the scanning root.
    osemgrep_force_project_root="targets/..",
)


@pytest.fixture
def run_semgrep_in_tmp(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> fixtures.RunSemgrep:
    """
    Note that this can cause failures if Semgrep pollutes either the targets or rules path
    """
    (tmp_path / "targets").symlink_to(TARGETS_PATH.resolve())
    (tmp_path / "rules").symlink_to(RULES_PATH.resolve())
    monkeypatch.chdir(tmp_path)

    return _run_strict_semgrep_on_basic_targets_with_json_output


@pytest.fixture
def run_semgrep_on_copied_files(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> fixtures.RunSemgrep:
    """
    Like run_semgrep_in_tmp, but fully copies rule and target data to avoid
    directory pollution, also avoids issues with symlink navigation
    """
    copytree(TARGETS_PATH.resolve(), tmp_path / "targets")
    copytree(RULES_PATH.resolve(), tmp_path / "rules")
    monkeypatch.chdir(tmp_path)

    return _run_strict_semgrep_on_basic_targets_with_json_output


@pytest.fixture
def git_tmp_path(monkeypatch: pytest.MonkeyPatch, tmp_path: Path):
    monkeypatch.chdir(tmp_path)
    # Initialize State
    subprocess.run(["git", "init"], check=True, capture_output=True)
    subprocess.run(
        ["git", "config", "user.email", "baselinetest@semgrep.com"],
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
def lockfile_path_in_tmp(monkeypatch: pytest.MonkeyPatch, tmp_path: Path):
    (tmp_path / "targets").symlink_to(TARGETS_PATH.resolve())
    (tmp_path / "rules").symlink_to(RULES_PATH.resolve())
    monkeypatch.chdir(tmp_path)


# similar to lockfile_path_in_tmp but with different targets path to save
# disk space (see performance/targets_perf_sca/readme.txt)
@pytest.fixture
def lockfile_path_in_tmp_for_perf(monkeypatch: pytest.MonkeyPatch, tmp_path: Path):
    (tmp_path / "targets_perf_sca").symlink_to(
        Path(TESTS_PATH / "performance" / "targets_perf_sca").resolve()
    )
    (tmp_path / "rules").symlink_to(RULES_PATH.resolve())
    monkeypatch.chdir(tmp_path)
