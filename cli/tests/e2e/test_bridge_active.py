import re

import pytest


@pytest.mark.kinda_slow
def test_bridge_log_msg(run_semgrep_in_tmp, tmp_path):
    """
    Verify that the bridge module is being used.
    """
    log_dest = tmp_path / "last.log"
    run_semgrep_in_tmp("rules/eqeq.yaml", env={"SEMGREP_LOG_FILE": str(log_dest)})

    log = log_dest.read_text()

    # Check that the CLI reports using the bridge module.  (Note: The
    # message for *loading* (rather than using) the module cannot be
    # checked here because it will instead be logged by a preceding test
    # if that test also runs semgrep without launching a new process.)
    regex = r"Running semgrep-core with command:\n[^\n]*semgrep_bridge_python.so"
    assert re.search(regex, log)
