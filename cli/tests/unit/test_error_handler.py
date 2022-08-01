import pytest
from pytest_mock import MockerFixture

from semgrep.error import FATAL_EXIT_CODE
from semgrep.error_handler import ErrorHandler


@pytest.fixture
def error_handler(mocker) -> ErrorHandler:
    return ErrorHandler()


class NetworkBlockedInTests(Exception):
    pass


@pytest.mark.quick
def test_send_nominal(error_handler, mocker: MockerFixture) -> None:
    """
    Check that data is posted to fail-open url and zero exit code is returned
    """
    expected_url = "https://fail-open.semgrep.dev/failure"
    expected_user_agent = "user-agent"

    mocked_state = mocker.MagicMock()
    mocked_state.app_session.user_agent = expected_user_agent
    mocked_state.env.fail_open_url = expected_url
    mocked_requests = mocker.patch("requests.post")
    mocker.patch("semgrep.state.get_state", return_value=mocked_state)

    error_handler.configure(suppress_errors=True)
    error_handler.push_request(
        "get", "https://semgrep.dev/api/agent/deployments/current"
    )
    exit_code = error_handler.send(FATAL_EXIT_CODE)

    expected_payload = {
        "headers": {"User-Agent": expected_user_agent},
        "method": "get",
        "url": "https://semgrep.dev/api/agent/deployments/current",
    }

    assert exit_code == 0
    mocked_requests.assert_called_once_with(
        expected_url, json=expected_payload, timeout=3
    )


@pytest.mark.quick
def test_send_nominal_with_trace(error_handler, mocker: MockerFixture) -> None:
    """
    Check that data is posted to fail-open url and zero exit code is returned
    """
    expected_url = "https://fail-open.semgrep.dev/failure"
    expected_user_agent = "user-agent"

    expected_traceback = "oops"
    error_handler.configure(suppress_errors=True)

    mocked_state = mocker.MagicMock()
    mocked_state.app_session.user_agent = expected_user_agent
    mocked_state.env.fail_open_url = expected_url
    mocked_requests = mocker.patch("requests.post")
    mocker.patch("sys.exc_info", return_value=(ValueError, "oops", "oops"))
    mocker.patch("traceback.format_exc", return_value=expected_traceback)
    mocker.patch("semgrep.state.get_state", return_value=mocked_state)

    try:
        raise ValueError()
    except Exception:
        pass
    finally:
        exit_code = error_handler.send(FATAL_EXIT_CODE)

    expected_payload = {
        "error": expected_traceback,
        "headers": {"User-Agent": expected_user_agent},
    }

    assert exit_code == 0
    mocked_requests.assert_called_once_with(
        expected_url, json=expected_payload, timeout=3
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
