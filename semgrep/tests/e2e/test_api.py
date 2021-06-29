import subprocess
import sys
from pathlib import Path

from semgrep.semgrep_main import invoke_semgrep


def test_api(capsys, run_semgrep_in_tmp):
    # Test that exposed python API works and prints out nothing to stderr or stdout
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

    # Check that logging code isnt handled by default root handler and printed to stderr
    x = subprocess.run(
        [
            sys.executable,
            "-c",
            "from semgrep.semgrep_main import invoke_semgrep; from pathlib import Path; invoke_semgrep(Path('rules/eqeq.yaml'),[Path('targets/bad/invalid_python.py'), Path('targets/basic/stupid.py')],)",
        ],
        encoding="utf-8",
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    assert x.stdout == ""
    assert x.stderr == ""
