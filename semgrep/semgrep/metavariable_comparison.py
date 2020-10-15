import json
import subprocess
import tempfile
from typing import Union

from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import SEMGREP_PATH
from semgrep.error import SemgrepError
from semgrep.util import sub_check_output


def metavariable_comparison(
    metavariable: str, comparison: str, content: Union[int, float, str]
) -> bool:
    core_request = {
        "metavars": {metavariable: content},
        "language": "python",  # Hardcode for now
        "code": comparison,
    }

    with tempfile.NamedTemporaryFile("w") as temp_file:
        json.dump(core_request, temp_file)
        temp_file.flush()
        cmd = [SEMGREP_PATH, "-eval", temp_file.name]
        try:
            output = sub_check_output(cmd)
        except subprocess.CalledProcessError as ex:
            raise SemgrepError(
                f"error invoking semgrep with:\n\t{' '.join(cmd)}\n\t{ex}\n{PLEASE_FILE_ISSUE_TEXT}"
            )

    return output.strip() == b"true"
