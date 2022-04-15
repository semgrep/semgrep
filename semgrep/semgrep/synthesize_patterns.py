import subprocess
from pathlib import Path
from typing import Sequence

from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.error import SemgrepError
from semgrep.semgrep_core import SemgrepCore
from semgrep.util import sub_check_output


def synthesize(
    language: str, code_to_synthesize: str, target_strings: Sequence[str]
) -> None:
    targets = [Path(target_string) for target_string in target_strings]

    if len(targets) != 1:
        raise SemgrepError("--synthesize-patterns requires exactly one target file")

    target = targets[0]
    args = ["-synthesize_patterns", code_to_synthesize, str(target)]

    cmd = [SemgrepCore.path()] + args
    try:
        output = sub_check_output(cmd)
    except subprocess.CalledProcessError as ex:
        raise SemgrepError(
            f"error invoking semgrep with:\n\t{' '.join(cmd)}\n\t{ex}\n{PLEASE_FILE_ISSUE_TEXT}"
        )
    print(output.decode(errors="replace"))
