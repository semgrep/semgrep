import subprocess
from typing import Sequence

import semgrep.config_resolver
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.error import SemgrepError
from semgrep.semgrep_core import SemgrepCore
from semgrep.util import sub_check_output


def synthesize(
    language: str, code_to_synthesize: str, targets_str: Sequence[str]
) -> None:
    targets = semgrep.config_resolver.resolve_targets(targets_str)

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
