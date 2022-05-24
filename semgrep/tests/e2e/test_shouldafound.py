from pathlib import Path
from shutil import copytree

import pytest
from click.testing import CliRunner

from semgrep.cli import cli
from semgrep.commands import scan
from semgrep.commands import shouldafound
from semgrep.constants import SEMGREP_SETTING_ENVVAR_NAME

# Point to the root of the tests dir
TESTS_PATH = Path(__file__).parent.parent


@pytest.mark.quick
def test_shouldafound_no_args(tmp_path, snapshot):
    """
    Test for shouldafound usage output
    """
    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
        }
    )
    result = runner.invoke(cli, ["shouldafound"])
    snapshot.assert_match(result.output, "shouldafound.txt")


@pytest.mark.quick
def test_shouldafound_no_confirmation(monkeypatch, snapshot, mocker, tmp_path):
    """
    Test that the -y flag allows seamless submission
    """
    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
        }
    )

    api_content = "https://foo.bar.semgrep.dev/playground/asdf"

    mocker.patch.object(
        shouldafound,
        "_make_shouldafound_request",
        return_value=api_content,
    )

    copytree(Path(TESTS_PATH / "e2e" / "targets").resolve(), tmp_path / "targets")
    copytree(Path(TESTS_PATH / "e2e" / "rules").resolve(), tmp_path / "rules")
    monkeypatch.chdir(tmp_path)

    result = runner.invoke(
        cli,
        [
            "shouldafound",
            "targets/basic/stupid.py",
            "--email",
            "foo@bar.com",
            "-m",
            "some vuln",
            "-y",
        ],
        env={},
    )

    assert result.exception == None
    assert result.exit_code == 0

    snapshot.assert_match(result.output, "shouldafound.txt")


@pytest.mark.quick
@pytest.mark.parametrize("pattern", ["11512123123", "$X == $X"])
@pytest.mark.parametrize("message", [None, "foobar"])
def test_shouldafound_findings_output(
    mocker, monkeypatch, tmp_path, snapshot, pattern, message
):
    """
    Test to ensure that semgrep scan with no findings DOES NOT show the
    """
    mocker.patch.object(
        scan,
        "get_no_findings_msg",
        return_value=message,
    )
    runner = CliRunner(
        env={
            SEMGREP_SETTING_ENVVAR_NAME: str(tmp_path),
        }
    )

    copytree(Path(TESTS_PATH / "e2e" / "targets").resolve(), tmp_path / "targets")
    copytree(Path(TESTS_PATH / "e2e" / "rules").resolve(), tmp_path / "rules")
    monkeypatch.chdir(tmp_path)

    result = runner.invoke(cli, ["-e", pattern, "-l", "python"], env={})

    assert result.exception == None
    assert result.exit_code == 0

    snapshot.assert_match(result.output, "output.txt")
