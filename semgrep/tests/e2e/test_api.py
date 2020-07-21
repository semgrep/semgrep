from pathlib import Path

from semgrep.semgrep_main import invoke_semgrep


def test_api(capsys, run_semgrep_in_tmp):
    # Test that exposed python API works and prints out nothing to stderr or stdout
    captured = capsys.readouterr()

    output = invoke_semgrep(
        Path("rules/eqeq.yaml"),
        [Path("targets/bad/invalid_python.py"), Path("targets/basic/stupid.py")],
    )

    assert len(output["errors"]) == 1
    assert len(output["results"]) == 1
    assert captured.out == ""
    assert captured.err == ""
