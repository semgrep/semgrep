import subprocess
from pathlib import Path
from shutil import copy
from shutil import copytree
from unittest.mock import ANY

import pytest
from click.testing import CliRunner

from semgrep.cli import cli
from semgrep.commands import scan
from semgrep.commands import shouldafound

# Point to the root of the tests dir
TESTS_PATH = Path(__file__).parent.parent


@pytest.mark.quick
def test_shouldafound_no_args(tmp_path, snapshot):
    """
    Test for shouldafound usage output
    """
    runner = CliRunner(env={"SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml")})
    result = runner.invoke(cli, ["shouldafound"])
    snapshot.assert_match(result.output, "shouldafound.txt")


@pytest.mark.quick
@pytest.mark.parametrize(
    "email_flag",
    [
        [],
        [
            "--email",
            "foo@bar.com",
        ],
    ],
)
@pytest.mark.parametrize(
    "message_flag",
    [
        [],
        [
            "-m",
            "some vuln",
        ],
    ],
)
@pytest.mark.parametrize("git_return_error", [True, False])
def test_shouldafound_no_confirmation(
    monkeypatch, git_return_error, email_flag, message_flag, snapshot, mocker, tmp_path
):
    """
    Test that the -y flag allows seamless submission
    """
    runner = CliRunner(env={"SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml")})

    api_content = "https://foo.bar.semgrep.dev/playground/asdf"

    request_mocker = mocker.patch.object(
        shouldafound,
        "_make_shouldafound_request",
        return_value=api_content,
    )

    path = "targets/basic/stupid.py"
    message = "some vuln"
    expected_email = "foo@bar.com"

    if git_return_error:
        mocker.patch.object(
            shouldafound,
            "_get_git_email",
            side_effect=subprocess.CalledProcessError(1, "mock"),
        )
    else:
        mocker.patch.object(shouldafound, "_get_git_email", return_value=expected_email)

    copytree(Path(TESTS_PATH / "e2e" / "targets").resolve(), tmp_path / "targets")
    copytree(Path(TESTS_PATH / "e2e" / "rules").resolve(), tmp_path / "rules")
    monkeypatch.chdir(tmp_path)

    should_prompt_for_email = len(email_flag) == 0

    args = [
        "shouldafound",
        path,
        "-y",
    ]

    args.extend(email_flag)
    args.extend(message_flag)

    result = runner.invoke(cli, args, input=f"{message}\n")

    expected = {
        # unit tests covering reading specific lines exist, don't test here
        "lines": ANY,
        "message": message,
        "path": path,
    }

    if git_return_error and len(email_flag) == 0:
        expected["email"] = None
    else:
        expected["email"] = expected_email

    request_mocker.assert_called_with(expected)

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

    copy(
        TESTS_PATH / "e2e" / "targets" / "basic" / "stupid.py",
        tmp_path / "stupid.py",
    )

    monkeypatch.chdir(tmp_path)

    mocker.patch.object(scan, "possibly_notify_user", return_value=None)

    runner = CliRunner(env={"SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml")})

    result = runner.invoke(
        cli,
        ["-e", pattern, "-l", "python"],
    )

    assert result.exception == None
    assert result.exit_code == 0

    snapshot.assert_match(result.output, "output.txt")
