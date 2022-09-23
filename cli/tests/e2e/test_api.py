import os
import subprocess
import sys
from pathlib import Path

import pytest

from semgrep.semgrep_main import invoke_semgrep


@pytest.mark.slow
def test_api(unique_home_dir, capsys, run_semgrep_in_tmp):
    # Test that exposed python API works and prints out nothing to stderr or stdout
    # unique_home_dir is used to ensure that the test runs with it's own
    # settings.yaml file to avoid reading one corrupted by another concurrent test run.
    output = invoke_semgrep(
        Path("rules/eqeq.yaml"),
        [Path("targets/bad/invalid_python.py"), Path("targets/basic/stupid.py")],
    )

    captured = capsys.readouterr()
    assert isinstance(output, dict)
    assert len(output["errors"]) == 1
    assert len(output["results"]) == 1
    assert captured.out == ""
    assert captured.err == ""


@pytest.mark.slow
def test_api_via_cli(unique_home_dir, run_semgrep_in_tmp):
    # Check that logging code isnt handled by default root handler and printed to stderr
    env = os.environ.copy()
    env["SEMGREP_SETTINGS_FILE"] = str(unique_home_dir / ".semgrep/settings.yaml")
    x = subprocess.run(
        [
            sys.executable,
            "-c",
            "from semgrep.semgrep_main import invoke_semgrep; from pathlib import Path; invoke_semgrep(Path('rules/eqeq.yaml'),[Path('targets/bad/invalid_python.py'), Path('targets/basic/stupid.py')],)",
        ],
        encoding="utf-8",
        capture_output=True,
        env=env,
    )
    assert x.stdout == ""
    assert x.stderr == ""
