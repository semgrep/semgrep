import pytest
from pytest_mock import MockerFixture

from semgrep.error import FATAL_EXIT_CODE
from semgrep.error_handler import ErrorHandler


FAKE_TOKEN = "abc123"
FAKE_USER_AGENT = "user-agent"
FAIL_OPEN_URL = "https://fail-open.semgrep.dev/failure"


@pytest.fixture
def error_handler(mocker) -> ErrorHandler:
    return ErrorHandler()


class NetworkBlockedInTests(Exception):
    pass


@pytest.fixture
def mock_get_token(mocker):
    mocked = mocker.patch("semgrep.app.auth.get_token", return_value=FAKE_TOKEN)
    yield mocked


@pytest.fixture
def mocked_state(mocker):
    mocked = mocker.MagicMock()
    mocked.app_session.user_agent = FAKE_USER_AGENT
    mocked.env.fail_open_url = FAIL_OPEN_URL.replace("/failure", "")
    mocker.patch("semgrep.state.get_state", return_value=mocked)
    yield mocked


@pytest.mark.quick
def test_send_nominal(
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
    exit_code = error_handler.send(FATAL_EXIT_CODE)

    expected_payload = {
        "method": "get",
        "url": "https://semgrep.dev/api/agent/deployments/current",
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
    mocker.patch("sys.exc_info", return_value=(ValueError, "oops", "oops"))
    mocker.patch("traceback.format_exc", return_value=expected_traceback)

    try:
        raise ValueError()
    except Exception:
        pass
    finally:
        exit_code = error_handler.send(FATAL_EXIT_CODE)

    expected_payload = {
        "error": expected_traceback,
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
def test_send_timeout_success(error_handler, mocker) -> None:
    """
    Check that no network does not cause failures and zero exit code is returned
    """
    mocker.patch("socket.getaddrinfo", side_effect=NetworkBlockedInTests)
    import requests

    # verify that network is blocked
    with pytest.raises(NetworkBlockedInTests):
        _ = requests.get("https://semgrep.dev", timeout=2)

    error_handler.configure(suppress_errors=True)
    exit_code = error_handler.send(FATAL_EXIT_CODE)

    assert exit_code == 0


@pytest.mark.quick
def test_send_skip(error_handler, mocker) -> None:
    """
    Check that data is NOT posted to fail-open url and fatal exit code is returned
    """
    mocked_requests = mocker.patch("requests.post")

    error_handler.configure(suppress_errors=False)
    exit_code = error_handler.send(FATAL_EXIT_CODE)

    assert exit_code == FATAL_EXIT_CODE
    mocked_requests.assert_not_called()
