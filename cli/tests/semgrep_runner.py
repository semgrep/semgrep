#
# Run either semgrep in the same process using Click or invoke the
# osemgrep command, which reimplements the semgrep CLI in OCaml.
#
# This is replacement for CliRunner provided by Click.
#
import os
import shlex
import sys
from subprocess import PIPE
from subprocess import Popen
from typing import Dict
from typing import List
from typing import Optional
from typing import Sequence
from typing import Union

from click.testing import CliRunner


def parse_env_bool(var_name: str, var_val: str) -> bool:
    if var_val == "true":
        return True
    elif var_val == "false":
        return False
    else:
        raise Exception(
            f"Environment variable {var_name}={var_val} "
            f"may not be assigned values other than 'true or 'false'."
        )


def get_env_bool(var_name: str) -> Optional[bool]:
    """Get the value of an environment holding either 'true' or 'false'."""
    s = os.environ.get(var_name)
    if s is None:
        return None
    else:
        return parse_env_bool(var_name, s)


# Environment variables that trigger the use of osemgrep
OSEMGREP_PATH = "osemgrep"
env_osemgrep = os.environ.get("PYTEST_OSEMGREP")
if env_osemgrep:
    OSEMGREP_PATH = env_osemgrep

USE_OSEMGREP = get_env_bool("PYTEST_USE_OSEMGREP")

# The --project-root option is used to prevent the .semgrepignore
# at the root of the git project to be taken into account when testing,
# which is a new behavior in osemgrep.
OSEMGREP_COMPATIBILITY_ARGS = ["--project-root", ".", "--experimental"]

# The semgrep command suitable to run semgrep as a separate process.
# It's something like ["semgrep"] or ["python3"; -m; "semgrep"] or
# ["/path/to/osemgrep"].
SEMGREP_BASE_COMMAND: List[str] = (
    [OSEMGREP_PATH] if USE_OSEMGREP else [sys.executable, "-m", "semgrep"]
)

SEMGREP_BASE_COMMAND_STR: str = " ".join(SEMGREP_BASE_COMMAND)


class Result:
    """Minimal properties of click.testing.Result used in our project.

    This is for compatibility with the test suite that uses a thing called
    Click to run a Python program without launching a new Python interpreter.
    Click is used by the tests to invoke 'semgrep' without incurring
    the 1-second startup delay each time the Python program starts. There's
    no such problem with the OCaml implementation but we must reproduce
    a result object that's sufficiently similar to what the Click invocation
    returns.
    https://click.palletsprojects.com/en/8.0.x/api/#click.testing.Result
    """

    def __init__(self, exit_code: int, stdout: str, stderr: str):
        self._exit_code = exit_code
        self._stdout = stdout
        self._stderr = stderr

    @property
    def stdout(self) -> str:
        return self._stdout

    @property
    def stderr(self) -> str:
        return self._stderr

    @property
    def output(self) -> str:
        """alias for stdout"""
        return self._stdout

    @property
    def exit_code(self) -> int:
        return self._exit_code


# Run osemgrep in an external process
# (there's no other choice since it's an OCaml binary)
def invoke_osemgrep(
    args: Optional[Union[str, Sequence[str]]], env: Optional[Dict[str, str]] = None
) -> Result:
    arg_list: List[str] = []
    if isinstance(args, str):
        # Parse the list of shell-quoted arguments
        arg_list = shlex.split(args)
    elif isinstance(args, List):
        arg_list = args
    argv: List[str] = []
    # ugly: adding --project-root for --help would trigger the wrong help message
    if "-h" in arg_list or "--help" in arg_list:
        argv = [OSEMGREP_PATH] + arg_list
    else:
        argv = [OSEMGREP_PATH] + OSEMGREP_COMPATIBILITY_ARGS + arg_list
    env_dict = {}
    if env:
        env_dict = env
    full_env = dict(os.environ, **env_dict)
    proc = Popen(argv, stdout=PIPE, stderr=PIPE, env=full_env)
    stdout, stderr = proc.communicate()
    return Result(proc.returncode, stdout.decode("utf-8"), stderr.decode("utf-8"))


class SemgrepRunner:
    """Run either semgrep with CliRunner or with osemgrep.

    It's meant as a drop-in replacement for CliRunner(cli, args).
    If a property is missing on the runner object, please add it here.
    """

    def __init__(self, env=None, mix_stderr=True):
        self._use_osemgrep = USE_OSEMGREP
        self._output = ""
        self._env = env
        self._mix_stderr = mix_stderr
        if not self._use_osemgrep:
            self._runner = CliRunner(env=env, mix_stderr=mix_stderr)

    def invoke(self, python_cli, args, input: Optional[str] = None, env=None) -> Result:
        if not self._use_osemgrep:
            result = self._runner.invoke(python_cli, args, input=input, env=env)
            stderr = result.stderr if not self._mix_stderr else ""
            return Result(result.exit_code, result.stdout, stderr)
        else:
            # TODO: do we need to support 'input' i.e. passing a string
            # to the program's stdin?
            if env:
                extra_env = dict(self._env, **env)
            else:
                extra_env = dict(self._env)
            return invoke_osemgrep(args, env=extra_env)
