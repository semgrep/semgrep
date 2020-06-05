import subprocess
import tempfile
from typing import List
from typing import Optional

import semgrep.config_resolver
from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import SEMGREP_PATH
from semgrep.error import SemgrepError


def dump_parsed_ast(
    to_json: bool, language: str, pattern: Optional[str], targets_str: List[str]
) -> None:
    targets = semgrep.config_resolver.resolve_targets(targets_str)

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

        cmd = [SEMGREP_PATH] + args
        try:
            output = subprocess.check_output(cmd, shell=False)
        except subprocess.CalledProcessError as ex:
            raise SemgrepError(
                f"error invoking semgrep with:\n\t{' '.join(cmd)}\n\t{ex}\n{PLEASE_FILE_ISSUE_TEXT}"
            )
        print(output.decode())
