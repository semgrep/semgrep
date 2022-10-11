#
# Run either semgrep in the same process using Click or inovke the
# osemgrep command, which reimplements the semgrep CLI in OCaml.
#
# This is replacement for CliRunner provided by Click.
#
import os
import sys
from subprocess import PIPE
from subprocess import Popen
from typing import Dict
from typing import List
from typing import Optional
from typing import Sequence
from typing import Union

from click.testing import CliRunner

# Environment variables that trigger the use of osemgrep
MLGREP_PATH = "osemgrep"
env_osemgrep = os.environ.get("PYTEST_MLGREP")
if env_osemgrep:
    MLGREP_PATH = env_osemgrep
USE_MLGREP = True if os.environ.get("PYTEST_USE_MLGREP") else False

# The semgrep command suitable to run semgrep as a separate process.
# It's something like ["semgrep"] or ["python3"; -m; "semgrep"] or
# ["/path/to/osemgrep"].
SEMGREP_BASE_COMMAND: List[str] = (
    [MLGREP_PATH] if USE_MLGREP else [sys.executable, "-m", "semgrep"]
)

SEMGREP_BASE_COMMAND_STR: str = " ".join(SEMGREP_BASE_COMMAND)


class Result:
    """Minimal properties of click.testing.Result used in our project.

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
        # This is not exactly how the shell would interpret the arguments
        # but hopefully it's sufficient to run our tests correctly.
        # If this doesn't work, consider changing the tests to they pass
        # arguments as a list rather than a string.
        arg_list = args.split(" ")
    elif isinstance(args, List):
        arg_list = args
    argv: List[str] = [MLGREP_PATH] + arg_list
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
        self._use_osemgrep = USE_MLGREP
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
            extra_env = dict(self._env, **env)
            return invoke_osemgrep(args, env=extra_env)
