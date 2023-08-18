##############################################################################
# Prelude
##############################################################################
# Small wrapper around 'semgrep' useful for writing end-to-end (e2e) tests.
#
# TODO: This file was originally introduced to optimize the way we were running
# semgrep in tests by using Click and its CliRunner to avoid an extra fork.
# However, with the introduction of osemgrep and the new cli/bin/semgrep (which
# dispatch to osemgrep), we actually want to avoid to use the CliRunner which
# only run pysemgrep. Otherwise, our e2e tests would not be really end-to
# -end and may not represent what the user would get by using semgrep directly.
# This is why using the CliRunner option is now deprecated. The option is still
# kept because a few of our tests still rely on Click-specific features that
# the regular call-semgrep-in-a-subprocess does not provide yet.
import os
import shlex
from dataclasses import dataclass
from pathlib import Path
from subprocess import PIPE
from subprocess import Popen
from typing import Dict
from typing import List
from typing import Optional
from typing import Sequence
from typing import Union

from click.testing import CliRunner

##############################################################################
# Constants
##############################################################################

# Environment variable that trigger the use of osemgrep
_USE_OSEMGREP = "PYTEST_USE_OSEMGREP" in os.environ

# The --project-root option is used to prevent the .semgrepignore
# at the root of the git project to be taken into account when testing,
# which is a new behavior in osemgrep.
_OSEMGREP_EXTRA_ARGS = ["--experimental", "--project-root", "."]

_SEMGREP_PATH = str((Path(__file__).parent.parent / "bin" / "semgrep").absolute())

# Exported constant, convenient to use in a list context.
SEMGREP_BASE_COMMAND: List[str] = (
    [_SEMGREP_PATH] + _OSEMGREP_EXTRA_ARGS if _USE_OSEMGREP else [_SEMGREP_PATH]
)

SEMGREP_BASE_COMMAND_STR: str = " ".join(SEMGREP_BASE_COMMAND)

##############################################################################
# Helpers
##############################################################################

# TODO: this should be removed as we don't want to run tests with Click
@dataclass
class Result:
    """Minimal properties of click.testing.Result used in our project.

    This is for compatibility with the test suite that uses Click
    to run a Python program without launching a new Python interpreter.
    Click is used by the tests to invoke 'semgrep' without incurring
    the 1-second startup delay each time the Python program starts. There's
    no such problem with the OCaml implementation but we must reproduce
    a result object that's sufficiently similar to what the Click invocation
    returns.
    https://click.palletsprojects.com/en/8.0.x/api/#click.testing.Result
    """

    exit_code: int
    stdout: str
    stderr: str

    @property
    def output(self) -> str:
        """alias for stdout"""
        return self.stdout


# Run semgrep in an external process
# TODO: right now it's forking osemgrep, but we should instead fork semgrep
# and use the --experimental flag when we want to force to use osemgrep
def fork_semgrep(
    args: Optional[Union[str, Sequence[str]]], env: Optional[Dict[str, str]] = None
) -> Result:

    # argv preparation
    arg_list: List[str] = []
    if isinstance(args, str):
        # Parse the list of shell-quoted arguments
        arg_list = shlex.split(args)
    elif isinstance(args, List):
        arg_list = args
    argv: List[str] = []

    # ugly: adding --project-root for --help would trigger the wrong help message
    if "-h" in arg_list or "--help" in arg_list:
        argv = [_SEMGREP_PATH] + arg_list
    else:
        argv = [_SEMGREP_PATH] + _OSEMGREP_EXTRA_ARGS + arg_list

    # env preparation
    env_dict = {}
    if env:
        env_dict = env
    full_env = dict(os.environ, **env_dict)

    # let's fork and use a pipe to communicate with the external semgrep
    proc = Popen(argv, stdout=PIPE, stderr=PIPE, env=full_env)
    stdout, stderr = proc.communicate()
    return Result(proc.returncode, stdout.decode("utf-8"), stderr.decode("utf-8"))


##############################################################################
# Entry point
##############################################################################


class SemgrepRunner:
    """Run either semgrep in a subprocess or with CliRunner.

    It's meant as a drop-in replacement for CliRunner(cli, args).
    If a property is missing on the runner object, please add it here.
    """

    def __init__(self, env=None, mix_stderr=True):
        self._use_osemgrep = _USE_OSEMGREP
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
            return fork_semgrep(args, env=extra_env)
