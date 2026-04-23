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
import uuid

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.app.scans import _ConfigPollTimeout
from semgrep.app.scans import ScanHandler


SEMGREP_URL = "https://semgrep.dev"
SCAN_REQUEST_ID = "12345678-1234-5678-1234-567812345678"

CREATE_SCAN_RESPONSE = {
    "info": {
        "id": 42,
        "enabled_products": [],
        "deployment_id": 1,
        "deployment_name": "test-org",
    }
}

SUCCESS_CONFIG_RESPONSE = {
    "status": "success",
    "config": {"rules": []},
    "engine_params": {},
}

PENDING_CONFIG_RESPONSE = {"status": "pending"}

FAILURE_CONFIG_RESPONSE = {"status": "failure"}


def _make_response(mocker, json_data, status_code=200):
    response = mocker.MagicMock()
    response.status_code = status_code
    response.json.return_value = json_data
    return response


@pytest.fixture
def mock_state(mocker):
    state = mocker.MagicMock()
    state.local_scan_id = uuid.UUID(SCAN_REQUEST_ID)
    state.env.semgrep_url = SEMGREP_URL
    state.env.sms_scan_id = None
    state.env.upload_findings_timeout = 30
    state.env.v2_poll_timeout_seconds = 45
    state.env.v2_post_max_attempts = 3
    state.env.v2_overall_timeout_minutes = 3
    mocker.patch("semgrep.app.scans.get_state", return_value=state)
    return state


@pytest.fixture
def mock_sleep(mocker):
    return mocker.patch("semgrep.app.scans.sleep")


@pytest.fixture
def handler(mock_state):
    return ScanHandler(enable_transitive_reachability=None)


@pytest.fixture
def mock_args(mocker):
    project_metadata = mocker.MagicMock()
    project_metadata.to_json.return_value = {}
    project_config = mocker.MagicMock()
    project_config.to_CiConfigFromRepo.return_value = None
    return project_metadata, project_config


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
def test_start_scan_v2_success_immediate(
    mocker, mock_state, mock_sleep, handler, mock_args
):
    """Returns ScanResponse immediately when config is ready on the first poll."""
    project_metadata, project_config = mock_args
    mock_state.app_session.post.return_value = _make_response(
        mocker, CREATE_SCAN_RESPONSE
    )
    mock_state.app_session.get.return_value = _make_response(
        mocker, SUCCESS_CONFIG_RESPONSE
    )

    result = handler.start_scan_v2(project_metadata, project_config)

    assert isinstance(result, out.ScanResponse)
    assert result.info.id == 42
    assert mock_state.app_session.get.call_count == 1
    mock_sleep.assert_not_called()


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
def test_start_scan_v2_success_after_pending(
    mocker, mock_state, mock_sleep, handler, mock_args
):
    """Polls through Pending responses before returning ScanResponse on Success."""
    project_metadata, project_config = mock_args
    mock_state.app_session.post.return_value = _make_response(
        mocker, CREATE_SCAN_RESPONSE
    )
    mock_state.app_session.get.side_effect = [
        _make_response(mocker, PENDING_CONFIG_RESPONSE),
        _make_response(mocker, PENDING_CONFIG_RESPONSE),
        _make_response(mocker, SUCCESS_CONFIG_RESPONSE),
    ]

    result = handler.start_scan_v2(project_metadata, project_config)

    assert isinstance(result, out.ScanResponse)
    assert mock_state.app_session.get.call_count == 3
    assert mock_sleep.call_count == 2


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
def test_start_scan_v2_config_failure(
    mocker, mock_state, mock_sleep, handler, mock_args
):
    """Raises immediately when the server signals config generation failed."""
    project_metadata, project_config = mock_args
    mock_state.app_session.post.return_value = _make_response(
        mocker, CREATE_SCAN_RESPONSE
    )
    mock_state.app_session.get.return_value = _make_response(
        mocker, FAILURE_CONFIG_RESPONSE
    )

    with pytest.raises(Exception, match="Config generation failed"):
        handler.start_scan_v2(project_metadata, project_config)

    assert mock_state.app_session.get.call_count == 1
    mock_sleep.assert_not_called()


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
def test_start_scan_uses_v2(mocker, mock_state, mock_sleep, mock_args):
    """start_scan always uses the v2 endpoint."""
    project_metadata, project_config = mock_args
    handler = ScanHandler(enable_transitive_reachability=None)
    mock_state.app_session.post.return_value = _make_response(
        mocker, CREATE_SCAN_RESPONSE
    )
    mock_state.app_session.get.return_value = _make_response(
        mocker, SUCCESS_CONFIG_RESPONSE
    )

    handler.start_scan(project_metadata, project_config)

    post_url = mock_state.app_session.post.call_args[0][0]
    assert post_url == f"{SEMGREP_URL}/api/cli/v2/scans"
    assert mock_state.app_session.get.call_count == 1
    get_url = mock_state.app_session.get.call_args[0][0]
    assert "/api/cli/v2/scans/" in get_url
    assert get_url.endswith("/config")


# ---------------------------------------------------------------------------
# POST-retry orchestration tests (mock _poll_for_config_v2 directly)
# ---------------------------------------------------------------------------


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
def test_start_scan_v2_retries_post_all_timeouts(
    mocker, mock_state, mock_sleep, handler, mock_args
):
    """Retries POST up to _V2_POST_MAX_ATTEMPTS times on repeated poll timeouts, then raises."""
    project_metadata, project_config = mock_args
    mock_state.app_session.post.return_value = _make_response(
        mocker, CREATE_SCAN_RESPONSE
    )
    mocker.patch.object(
        handler, "_poll_for_config_v2", side_effect=_ConfigPollTimeout("timed out")
    )

    with pytest.raises(Exception, match="timed out after 3 POST attempts"):
        handler.start_scan_v2(project_metadata, project_config)

    assert mock_state.app_session.post.call_count == 3
    assert handler._poll_for_config_v2.call_count == 3


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
def test_start_scan_v2_success_on_second_post_attempt(
    mocker, mock_state, mock_sleep, handler, mock_args
):
    """Returns successfully when the second POST attempt's poll resolves."""
    project_metadata, project_config = mock_args
    mock_state.app_session.post.return_value = _make_response(
        mocker, CREATE_SCAN_RESPONSE
    )
    scan_response = mocker.MagicMock()
    mocker.patch.object(
        handler,
        "_poll_for_config_v2",
        side_effect=[_ConfigPollTimeout("timed out"), scan_response],
    )

    result = handler.start_scan_v2(project_metadata, project_config)

    assert result is scan_response
    assert mock_state.app_session.post.call_count == 2
    assert handler._poll_for_config_v2.call_count == 2


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
def test_start_scan_v2_failure_does_not_retry_post(
    mocker, mock_state, mock_sleep, handler, mock_args
):
    """Does not retry the POST when the poll raises a hard failure (not _ConfigPollTimeout)."""
    project_metadata, project_config = mock_args
    mock_state.app_session.post.return_value = _make_response(
        mocker, CREATE_SCAN_RESPONSE
    )
    mocker.patch.object(
        handler,
        "_poll_for_config_v2",
        side_effect=Exception("Config generation failed"),
    )

    with pytest.raises(Exception, match="Config generation failed"):
        handler.start_scan_v2(project_metadata, project_config)

    assert mock_state.app_session.post.call_count == 1


# ---------------------------------------------------------------------------
# _poll_for_config_v2 tests
# ---------------------------------------------------------------------------


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
def test_poll_for_config_v2_raises_config_poll_timeout_when_expired(
    mocker, mock_state, handler
):
    """Raises _ConfigPollTimeout immediately when timeout_seconds=0 (deadline already past)."""
    scan_info = mocker.MagicMock()
    mock_state.app_session.get.return_value = _make_response(
        mocker, PENDING_CONFIG_RESPONSE
    )

    with pytest.raises(_ConfigPollTimeout):
        handler._poll_for_config_v2(SCAN_REQUEST_ID, scan_info, timeout_seconds=0)

    mock_state.app_session.get.assert_not_called()
