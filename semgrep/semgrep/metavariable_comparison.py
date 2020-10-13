import collections
import json
import subprocess
import tempfile
from typing import Union

from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import SEMGREP_PATH
from semgrep.error import SemgrepError
from semgrep.util import sub_check_output


def metavariable_comparison(
    metavariable: str, comparison: str, content: Union[int, float]
) -> bool:
    # semgrep-core requires the 'language' key be before the 'code' key,
    # so we must use an OrderedDict with special ordering here
    core_request = collections.OrderedDict()
    core_request["metavars"] = {metavariable: str(content)}
    core_request["language"] = "python"  # Hardcode for now
    core_request["code"] = comparison

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
