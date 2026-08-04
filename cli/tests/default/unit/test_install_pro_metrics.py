#
# Copyright (c) 2026 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
# Tests for the metrics sent by `semgrep install-semgrep-pro`:
# - the install-method heuristic (environment.installMethod)
# - the install_pro payload section (success/error)
# - that the command sends (or does not send) metrics depending on
#   login status and --metrics
#
import subprocess

import pytest
import requests
from click.testing import CliRunner

from semgrep.cli import cli
from semgrep.commands import install
from semgrep.metrics import get_install_method
from semgrep.metrics import Metrics
from semgrep.semgrep_core import SemgrepCore
from semgrep.state import SemgrepState

FAKE_TOKEN = "abc123"


@pytest.fixture(autouse=True)
def isolated_env(tmp_path, monkeypatch):
    # Never inherit metrics/auth configuration from the environment running
    # the tests, and keep settings.yml writes out of the real home directory.
    monkeypatch.delenv("SEMGREP_SEND_METRICS", raising=False)
    monkeypatch.delenv("SEMGREP_APP_TOKEN", raising=False)
    monkeypatch.delenv("SEMGREP_IN_DOCKER", raising=False)
    monkeypatch.setenv("SEMGREP_SETTINGS_FILE", str(tmp_path / "settings.yml"))


##############################################################################
# Install-method heuristic
##############################################################################


@pytest.mark.quick
@pytest.mark.parametrize(
    "path, expected",
    [
        (
            "/opt/homebrew/Cellar/semgrep/1.100.0/libexec/lib/python3.12/site-packages/semgrep/metrics.py",
            "homebrew",
        ),
        (
            "/home/linuxbrew/.linuxbrew/Cellar/semgrep/1.100.0/libexec/lib/python3.12/site-packages/semgrep/metrics.py",
            "homebrew",
        ),
        (
            "/home/user/.venv/lib/python3.12/site-packages/semgrep/metrics.py",
            "pip",
        ),
        (
            "/usr/lib/python3/dist-packages/semgrep/metrics.py",
            "pip",
        ),
        (
            "/home/user/code/semgrep/cli/src/semgrep/metrics.py",
            "unknown",
        ),
        (
            r"C:\Users\user\AppData\Local\Programs\Python\Python312\Lib\site-packages\semgrep\metrics.py",
            "pip",
        ),
        (
            r"C:\Users\user\project\.venv\Lib\site-packages\semgrep\metrics.py",
            "pip",
        ),
        (
            r"C:\Users\user\code\semgrep\cli\src\semgrep\metrics.py",
            "unknown",
        ),
    ],
)
def test_get_install_method_from_path(path, expected) -> None:
    assert get_install_method(path) == expected


@pytest.mark.quick
def test_get_install_method_docker_wins(monkeypatch) -> None:
    monkeypatch.setenv("SEMGREP_IN_DOCKER", "1")
    pip_path = "/usr/lib/python3.12/site-packages/semgrep/metrics.py"
    assert get_install_method(pip_path) == "docker"


@pytest.mark.quick
def test_environment_install_method_in_payload(monkeypatch) -> None:
    monkeypatch.setenv("SEMGREP_IN_DOCKER", "1")
    metrics = Metrics()
    assert metrics.payload.environment.installMethod == "docker"
    assert '"installMethod": "docker"' in metrics.as_json()


##############################################################################
# install_pro payload section
##############################################################################


@pytest.mark.quick
def test_add_install_pro_fields() -> None:
    metrics = Metrics()
    assert metrics.payload.install_pro.success is None
    assert metrics.payload.install_pro.error is None

    metrics.stage_install_pro_error("download-401")
    metrics.add_install_pro_outcome(False)

    assert metrics.payload.install_pro.success is False
    assert metrics.payload.install_pro.error == "download-401"
    as_json = metrics.as_json()
    assert '"success": false' in as_json
    assert '"download-401"' in as_json


@pytest.mark.quick
def test_clear_install_pro_error() -> None:
    metrics = Metrics()

    metrics.stage_install_pro_error("final-install-failed")
    assert metrics.payload.install_pro.error == "final-install-failed"

    metrics.clear_install_pro_error()
    assert metrics.payload.install_pro.error is None


##############################################################################
# run_install_semgrep_pro instrumentation
##############################################################################


@pytest.fixture
def fake_state(tmp_path, mocker) -> SemgrepState:
    """A fresh logged-in state, with paths pointed into tmp_path."""
    state = SemgrepState()
    state.app_session.token = FAKE_TOKEN
    mocker.patch.object(install, "get_state", return_value=state)
    mocker.patch.object(
        SemgrepCore, "path", return_value=str(tmp_path / "semgrep-core")
    )
    mocker.patch.object(
        SemgrepCore, "pro_version_stamp_path", return_value=tmp_path / "pro-stamp"
    )
    return state


@pytest.mark.quick
def test_run_install_success_records_outcome(fake_state, mocker) -> None:
    mocker.patch.object(
        install,
        "download_semgrep_pro",
        side_effect=lambda _state, _platform, dest: dest.write_bytes(b"binary"),
    )
    mocker.patch.object(install, "sub_check_output", return_value="1.2.3\n")
    mocker.patch("os.chmod")

    install.run_install_semgrep_pro()

    assert fake_state.metrics.payload.install_pro.success is True
    assert fake_state.metrics.payload.install_pro.error is None


@pytest.mark.quick
def test_run_install_records_deployment_id(fake_state, mocker) -> None:
    mocker.patch.object(install, "get_deployment_id", return_value=1234)
    mocker.patch.object(
        install,
        "download_semgrep_pro",
        side_effect=lambda _state, _platform, dest: dest.write_bytes(b"binary"),
    )
    mocker.patch.object(install, "sub_check_output", return_value="1.2.3\n")
    mocker.patch("os.chmod")

    install.run_install_semgrep_pro()

    assert fake_state.metrics.payload.environment.deployment_id == 1234
    assert '"deployment_id": 1234' in fake_state.metrics.as_json()


@pytest.mark.quick
def test_run_install_no_token_records_error(fake_state) -> None:
    fake_state.app_session.token = None

    with pytest.raises(SystemExit):
        install.run_install_semgrep_pro()

    assert fake_state.metrics.payload.install_pro.error == "no-api-token"


@pytest.mark.quick
@pytest.mark.parametrize(
    "exception",
    [
        # binary starts but dies, e.g. dynamic linking failed after exec
        subprocess.CalledProcessError(1, "semgrep-core-proprietary"),
        # binary cannot even start, e.g. exec format error on the wrong arch
        OSError(8, "Exec format error"),
        # binary hangs
        subprocess.TimeoutExpired("semgrep-core-proprietary", 10),
    ],
)
def test_run_install_version_check_failure_records_error(
    fake_state, mocker, exception
) -> None:
    mocker.patch.object(
        install,
        "download_semgrep_pro",
        side_effect=lambda _state, _platform, dest: dest.write_bytes(b"binary"),
    )
    mocker.patch.object(install, "sub_check_output", side_effect=exception)
    mocker.patch("os.chmod")

    with pytest.raises(SystemExit):
        install.run_install_semgrep_pro()

    assert fake_state.metrics.payload.install_pro.error == "version-check-failed"


@pytest.mark.quick
@pytest.mark.parametrize(
    "status_code, expected_error",
    [(401, "download-401"), (403, "download-403")],
)
def test_download_http_auth_failures(
    fake_state, mocker, tmp_path, status_code, expected_error
) -> None:
    response = mocker.MagicMock()
    response.status_code = status_code
    get_mock = mocker.patch.object(fake_state.app_session, "get")
    get_mock.return_value.__enter__.return_value = response

    with pytest.raises(SystemExit):
        install.download_semgrep_pro(fake_state, "manylinux", tmp_path / "download")

    assert fake_state.metrics.payload.install_pro.error == expected_error


##############################################################################
# Command-level: does install-semgrep-pro post metrics?
##############################################################################


@pytest.fixture
def mock_install_steps(tmp_path, mocker):
    """Mock the individual install steps so the real run_install_semgrep_pro
    logic (including its metrics instrumentation) still runs."""
    mocker.patch.object(
        SemgrepCore, "path", return_value=str(tmp_path / "semgrep-core")
    )
    mocker.patch.object(
        SemgrepCore, "pro_version_stamp_path", return_value=tmp_path / "pro-stamp"
    )
    mocker.patch.object(
        install,
        "download_semgrep_pro",
        side_effect=lambda _state, _platform, dest: dest.write_bytes(b"binary"),
    )
    mocker.patch.object(install, "sub_check_output", return_value="1.2.3\n")
    mocker.patch("os.chmod")


@pytest.fixture
def post_metrics_mock(mocker):
    return mocker.patch.object(Metrics, "_post_metrics", autospec=True)


@pytest.mark.quick
def test_install_command_sends_metrics_when_logged_in(
    monkeypatch, mock_install_steps, post_metrics_mock
) -> None:
    monkeypatch.setenv("SEMGREP_APP_TOKEN", FAKE_TOKEN)

    result = CliRunner().invoke(cli, ["install-semgrep-pro"])

    assert result.exit_code == 0
    post_metrics_mock.assert_called_once()
    # autospec=True passes the Metrics instance as the first positional arg
    payload = post_metrics_mock.call_args.args[0].payload
    assert "subcommand/install-semgrep-pro" in payload.value.features
    assert payload.install_pro.success is True
    assert payload.install_pro.error is None
    assert payload.errors.returnCode == 0
    assert payload.environment.installMethod is not None


@pytest.mark.quick
def test_install_command_sends_failure_metrics(
    monkeypatch, mock_install_steps, post_metrics_mock, mocker
) -> None:
    monkeypatch.setenv("SEMGREP_APP_TOKEN", FAKE_TOKEN)
    mocker.patch.object(
        install,
        "sub_check_output",
        side_effect=subprocess.CalledProcessError(1, "semgrep-core-proprietary"),
    )

    result = CliRunner().invoke(cli, ["install-semgrep-pro"])

    assert result.exit_code == 2
    post_metrics_mock.assert_called_once()
    payload = post_metrics_mock.call_args.args[0].payload
    assert payload.install_pro.success is False
    assert payload.install_pro.error == "version-check-failed"
    assert payload.errors.returnCode == 2


@pytest.mark.quick
def test_install_command_unexpected_exception_records_unknown(
    monkeypatch, mock_install_steps, post_metrics_mock, mocker
) -> None:
    # An exception raised before any step reason is staged (here, while
    # determining the install path) has nothing to attribute it to, so the
    # outcome falls back to "unknown".
    monkeypatch.setenv("SEMGREP_APP_TOKEN", FAKE_TOKEN)
    mocker.patch.object(
        install, "determine_semgrep_pro_path", side_effect=RuntimeError("boom")
    )

    result = CliRunner().invoke(cli, ["install-semgrep-pro"])

    assert result.exit_code == 2
    post_metrics_mock.assert_called_once()
    payload = post_metrics_mock.call_args.args[0].payload
    assert payload.install_pro.success is False
    assert payload.install_pro.error == "unknown"


@pytest.mark.quick
@pytest.mark.parametrize(
    "exception",
    [
        # a requests failure, the previously-handled case
        requests.ConnectionError("connection reset"),
        # any other exception is now attributed to the download step too,
        # rather than falling back to "unknown"
        RuntimeError("boom"),
    ],
)
def test_install_command_download_error_records_download_reason(
    monkeypatch, mock_install_steps, post_metrics_mock, mocker, exception
) -> None:
    monkeypatch.setenv("SEMGREP_APP_TOKEN", FAKE_TOKEN)
    mocker.patch.object(
        install,
        "download_semgrep_pro",
        side_effect=exception,
    )

    result = CliRunner().invoke(cli, ["install-semgrep-pro"])

    assert result.exit_code == 2
    payload = post_metrics_mock.call_args.args[0].payload
    assert payload.install_pro.success is False
    assert payload.install_pro.error == "download-failed"


@pytest.mark.quick
def test_install_command_metrics_off(
    monkeypatch, mock_install_steps, post_metrics_mock
) -> None:
    monkeypatch.setenv("SEMGREP_APP_TOKEN", FAKE_TOKEN)

    result = CliRunner().invoke(cli, ["install-semgrep-pro", "--metrics", "off"])

    assert result.exit_code == 0
    post_metrics_mock.assert_not_called()


@pytest.mark.quick
def test_install_command_no_metrics_when_logged_out(
    mock_install_steps, post_metrics_mock
) -> None:
    # not logged in: the command fails early, and in AUTO mode metrics must
    # not be sent for unauthenticated runs
    result = CliRunner().invoke(cli, ["install-semgrep-pro"])

    assert result.exit_code != 0
    post_metrics_mock.assert_not_called()
