import subprocess
import tempfile
from typing import List
from typing import Optional

import semgrep.config_resolver
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import SEMGREP_PATH
from semgrep.error import SemgrepError


def synthesize_patterns(
    language: str, code_to_synthesize: str, targets_str: List[str]
) -> None:
    targets = semgrep.config_resolver.resolve_targets(targets_str)

    if len(targets) != 1:
        raise SemgrepError("--synthesize-patterns requires exactly one target file")

    target = targets[0]
    args = ["-synthesize_patterns", code_to_synthesize, str(target)]

    cmd = [SEMGREP_PATH] + args
    try:
        output = subprocess.check_output(cmd, shell=False)
    except subprocess.CalledProcessError as ex:
        raise SemgrepError(
            f"error invoking semgrep with:\n\t{' '.join(cmd)}\n\t{ex}\n{PLEASE_FILE_ISSUE_TEXT}"
        )
    print(output.decode())
