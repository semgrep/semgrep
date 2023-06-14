import os
import subprocess
import sys
from pathlib import Path

import pytest
from tests.fixtures import RunSemgrep

from semgrep.semgrep_main import invoke_semgrep

# When calling osemgrep, stderr isn't available via this 'capsys' object,
# causing the test to pass when it shouldn't.
# TODO: use SemgrepRunner instead, which is drop-in replacement for CliRunner.
@pytest.mark.slow
@pytest.mark.todo
def test_api(unique_home_dir, capsys, run_semgrep_in_tmp: RunSemgrep):
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
@pytest.mark.no_semgrep_cli
def test_api_via_cli(unique_home_dir, run_semgrep_in_tmp: RunSemgrep):
    # Check that logging code isnt handled by default root handler and printed to stderr
    # This is run as a separate test from the one above so that it has a separate, temp directory
    env = os.environ.copy()
    # Assign env var for settings.yaml to the per-test unique home directory
    # so it doesn't use the default (~/.semgrep/settings.yaml)
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
