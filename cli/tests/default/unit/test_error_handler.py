from uuid import uuid4

import pytest
import requests
from pytest_mock import MockerFixture
from requests.exceptions import ConnectionError
from tests.conftest import str_containing

from semgrep.commands.wrapper import handle_command_errors
from semgrep.error import FATAL_EXIT_CODE
from semgrep.error_handler import ErrorHandler


FAKE_TOKEN = "abc123"
FAKE_USER_AGENT = "user-agent"
FAIL_OPEN_URL = "https://fail-open.semgrep.dev/failure"


@pytest.fixture
def error_handler_enabled() -> bool:
    return True


@pytest.fixture
def error_handler(error_handler_enabled) -> ErrorHandler:
    error_handler = ErrorHandler()
    error_handler.configure(suppress_errors=error_handler_enabled)
    return error_handler


class NetworkBlockedInTests(Exception):
    pass


@pytest.fixture
def mock_get_token(mocker):
    mocked = mocker.patch("semgrep.app.auth.get_token", return_value=FAKE_TOKEN)
    yield mocked


@pytest.fixture(autouse=True)
def mocked_state(mocker, error_handler):
    mocked = mocker.MagicMock()
    mocked.app_session.user_agent = FAKE_USER_AGENT
    mocked.local_scan_id = uuid4()
    mocked.env.fail_open_url = FAIL_OPEN_URL.replace("/failure", "")
    mocked.error_handler = error_handler
    mocker.patch("semgrep.state.get_state", return_value=mocked)
    yield mocked


@pytest.fixture(autouse=True)
def mock_broken_request(requests_mock):
    return requests_mock.get(
        "https://semgrep.dev/api/agent/deployments/current", exc=ConnectionError
    )


@pytest.fixture
def fail_open_mock(requests_mock):
    return requests_mock.post(FAIL_OPEN_URL)


@handle_command_errors
def fake_command():
    requests.get("https://semgrep.dev/api/agent/deployments/current")


@pytest.mark.quick
def test_send_nominal(fail_open_mock) -> None:
    """
    Check that data is posted to fail-open url and zero exit code is returned
    """
    with pytest.raises(SystemExit) as exit_exc:
        fake_command()

    assert exit_exc.type == SystemExit
    assert exit_exc.value.code == 0

    assert fail_open_mock.called
    payload = fail_open_mock.last_request.json()
    assert payload["error"] == str_containing("Traceback")
    assert payload["exit_code"] == 2


@pytest.mark.quick
def test_send_timeout_success(requests_mock) -> None:
    """
    Check that no network does not cause failures and zero exit code is returned
    """
    fail_open_mock = requests_mock.post(FAIL_OPEN_URL, exc=ConnectionError)

    with pytest.raises(SystemExit) as exit_exc:
        fake_command()

    assert exit_exc.type == SystemExit
    assert exit_exc.value.code == 0
    assert fail_open_mock.called


@pytest.mark.quick
@pytest.mark.parametrize("error_handler_enabled", [False])
def test_send_skip(fail_open_mock) -> None:
    """
    Check that data is NOT posted to fail-open url and fatal exit code is returned
    """
    with pytest.raises(SystemExit) as exit_exc:
        fake_command()

    assert exit_exc.type == SystemExit
    assert exit_exc.value.code == 2
    assert not fail_open_mock.called


## the rest of these tests might require more though, or just to be removed.


@pytest.mark.quick
def test_send_with_scan_id(
    error_handler, mocker: MockerFixture, mock_get_token, mocked_state
) -> None:
    """
    Check that data is posted to fail-open url and zero exit code is returned
    """
    mocked_requests = mocker.patch("requests.post")

    error_handler.configure(suppress_errors=True)
    error_handler.push_request(
        "get", "https://semgrep.dev/api/agent/deployments/current"
    )
    scan_id = 1234
    error_handler.append_request(scan_id=scan_id)
    exit_code = error_handler.send(FATAL_EXIT_CODE)

    expected_payload = {
        "method": "get",
        "url": "https://semgrep.dev/api/agent/deployments/current",
        "scan_id": 1234,
        "request_id": str(mocked_state.local_scan_id),
        "exit_code": 2,
    }

    expected_headers = {
        "User-Agent": FAKE_USER_AGENT,
        "Authorization": f"Bearer {FAKE_TOKEN}",
    }

    assert exit_code == 0
    mocked_requests.assert_called_once_with(
        FAIL_OPEN_URL,
        headers=expected_headers,
        json=expected_payload,
        timeout=3,
    )


@pytest.mark.quick
def test_send_nominal_with_trace(
    error_handler, mocker: MockerFixture, mock_get_token, mocked_state
) -> None:
    """
    Check that data is posted to fail-open url and zero exit code is returned
    """

    expected_traceback = "oops"
    error_handler.configure(suppress_errors=True)

    mocked_requests = mocker.patch("requests.post")
    mocker.patch("traceback.format_exc", return_value=expected_traceback)

    try:
        raise ValueError()
    except Exception:
        error_handler.capture_error()
    finally:
        exit_code = error_handler.send(FATAL_EXIT_CODE)

    expected_payload = {
        "error": expected_traceback,
        "request_id": str(mocked_state.local_scan_id),
        "exit_code": 2,
    }
    expected_headers = {
        "User-Agent": FAKE_USER_AGENT,
        "Authorization": f"Bearer {FAKE_TOKEN}",
    }

    assert exit_code == 0
    mocked_requests.assert_called_once_with(
        FAIL_OPEN_URL,
        headers=expected_headers,
        json=expected_payload,
        timeout=3,
    )
