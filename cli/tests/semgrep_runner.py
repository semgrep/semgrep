#
# Run either semgrep in the same process using Click or inovke the
# mlgrep command, which reimplements the semgrep CLI in OCaml.
#
# This is replacement for CliRunner provided by Click.
#
import os
from subprocess import PIPE
from subprocess import Popen
from typing import List
from typing import Optional

from click.testing import CliRunner

MLGREP_PATH = "mlgrep"
USE_MLGREP = True if os.environ.get("PYTEST_USE_MLGREP") else False


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


# Run mlgrep in an external process (since it's an OCaml binary).
def invoke_mlgrep(args: List[str]) -> Result:
    argv = [MLGREP_PATH] + args
    proc = Popen(argv, stdout=PIPE, stderr=PIPE)
    stdout, stderr = proc.communicate()
    return Result(proc.returncode, stdout.decode("utf-8"), stderr.decode("utf-8"))


class SemgrepRunner:
    """Run either semgrep with CliRunner or with mlgrep.

    It's meant as a drop-in replacement for CliRunner(cli, args).
    If a property is missing on the runner object, please add it here.
    """

    def __init__(self, env=None, mix_stderr=False):
        self._use_mlgrep = USE_MLGREP
        self._output = ""
        self.exit_code = -1000
        if not self._use_mlgrep:
            self._runner = CliRunner(env=env, mix_stderr=mix_stderr)

    def invoke(self, python_cli, args, input: Optional[str] = None) -> Result:
        if not self._use_mlgrep:
            result = self._runner.invoke(python_cli, args, input=input)
            return Result(result.exit_code, result.stdout, result.stderr)
        else:
            # TODO: do we need to support 'input' i.e. passing a string
            # to the program's stdin?
            return invoke_mlgrep(args)
