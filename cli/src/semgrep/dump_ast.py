import subprocess
import tempfile
from pathlib import Path
from typing import Optional
from typing import Sequence

from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.error import SemgrepError
from semgrep.semgrep_core import SemgrepCore
from semgrep.util import sub_check_output


def dump_parsed_ast(
    to_json: bool, language: str, pattern: Optional[str], target_strings: Sequence[str]
) -> None:
    targets = [Path(target_string) for target_string in target_strings]

    with tempfile.NamedTemporaryFile("w") as fout:
        args = []
        if pattern:
            fout.write(pattern)
            fout.flush()
            args = ["-lang", language, "-dump_pattern", fout.name]
        else:
            if len(targets) != 1:
                raise SemgrepError("--dump-ast requires exactly one target file")
            target = targets[0]
            args = ["-lang", language, "-dump_ast", str(target)]

        if to_json:
            args = ["-json"] + args

        # Use the executable even if the module is available.  This
        # one-off usage does not employ the usual progress protocol on
        # stdout, and does different things with the child's stderr than
        # the normal case (namely, it discards it or passes it through
        # depending on --quiet).  StreamingSemgrepCore could be extended
        # with these capabilities in the future if desired.
        cmd = [SemgrepCore.executable_path()] + args
        try:
            output = sub_check_output(cmd)
        except subprocess.CalledProcessError as ex:
            raise SemgrepError(
                f"error invoking semgrep with:\n\t{' '.join(cmd)}\n\t{ex}\n{PLEASE_FILE_ISSUE_TEXT}"
            )
        print(output.decode(errors="replace"))
