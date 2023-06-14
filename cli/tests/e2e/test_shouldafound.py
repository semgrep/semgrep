import subprocess
from pathlib import Path
from shutil import copy
from shutil import copytree
from unittest.mock import ANY  # nosem: use-pytest-mock

import pytest
from tests.fixtures import RunSemgrep

from semgrep.commands import scan
from semgrep.commands import shouldafound

# Point to the root of the tests dir
TESTS_PATH = Path(__file__).parent.parent


@pytest.mark.quick
def test_shouldafound_no_args(run_semgrep: RunSemgrep, tmp_path, snapshot):
    """
    Test for shouldafound usage output
    """
    snapshot.assert_match(
        run_semgrep(options=["shouldafound"], assert_exit_code=None).as_snapshot(),
        "results.txt",
    )


@pytest.mark.quick
@pytest.mark.parametrize(
    "email_args",
    [
        [],
        [
            "--email",
            "foo@bar.com",
        ],
    ],
)
@pytest.mark.parametrize(
    "message_args",
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
    monkeypatch,
    run_semgrep: RunSemgrep,
    git_return_error,
    email_args,
    message_args,
    snapshot,
    mocker,
    tmp_path,
):
    """
    Test that the -y flag allows seamless submission
    """
    request_mock = mocker.patch.object(
        shouldafound,
        "_make_shouldafound_request",
        return_value="https://foo.bar.semgrep.dev/playground/asdf",
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

    results = run_semgrep(
        options=["shouldafound", path, "-y", *email_args, *message_args],
        stdin=f"{message}\n",
        env={"SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml")},
        assert_exit_code=None,
    )

    request_mock.assert_called_with(
        {
            # unit tests covering reading specific lines exist, don't test here
            "lines": ANY,
            "message": message,
            "path": path,
            "email": expected_email if not git_return_error or email_args else None,
        }
    )

    snapshot.assert_match(results.as_snapshot(), "results.txt")


@pytest.mark.quick
@pytest.mark.parametrize("pattern", ["11512123123", "$X == $X"])
@pytest.mark.parametrize("message", [None, "foobar"])
def test_shouldafound_findings_output(
    run_semgrep: RunSemgrep, mocker, monkeypatch, tmp_path, snapshot, pattern, message
):
    """
    Test to ensure that semgrep scan with no findings DOES NOT show the
    """
    mocker.patch.object(scan, "get_no_findings_msg", return_value=message)

    copy(
        TESTS_PATH / "e2e" / "targets" / "basic" / "stupid.py",
        tmp_path / "stupid.py",
    )

    monkeypatch.chdir(tmp_path)

    mocker.patch.object(scan, "possibly_notify_user", return_value=None)

    results = run_semgrep(
        options=["scan", "-e", pattern, "-l", "python"],
        env={"SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml")},
        assert_exit_code=None,
    )

    snapshot.assert_match(results.as_snapshot(), "results.txt")
