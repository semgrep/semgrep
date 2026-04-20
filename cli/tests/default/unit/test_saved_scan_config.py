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
from semgrep.app.scans import ScanHandler


SEMGREP_URL = "https://semgrep.dev"
SCAN_REQUEST_ID = "12345678-1234-5678-1234-567812345678"

V1_SCAN_RESPONSE = {
    "info": {
        "id": 42,
        "enabled_products": [],
        "deployment_id": 1,
        "deployment_name": "test-org",
    },
    "config": {"rules": []},
    "engine_params": {},
}


@pytest.fixture
def mock_state(mocker):
    state = mocker.MagicMock()
    state.local_scan_id = uuid.UUID(SCAN_REQUEST_ID)
    state.env.semgrep_url = SEMGREP_URL
    state.env.sms_scan_id = None
    state.env.upload_findings_timeout = 30
    mocker.patch("semgrep.app.scans.get_state", return_value=state)
    return state


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
def test_dump_and_load_scan_config_roundtrip(mocker, mock_state, tmp_path):
    """ScanResponse can be saved to disk and loaded back with all fields intact."""
    config_path = tmp_path / "scan_config.json"

    # Create a handler that will dump the config when handling a scan response
    handler = ScanHandler(
        enable_transitive_reachability=None,
        dump_scan_config_path=config_path,
    )
    scan_response = out.ScanResponse.from_json(V1_SCAN_RESPONSE)
    handler._handle_scan_response(scan_response)
    assert config_path.exists()

    # Create a second handler that loads the saved config via start_scan
    mocker.patch("semgrep.app.scans.telemetry")
    handler2 = ScanHandler(
        enable_transitive_reachability=None,
        load_saved_scan_config_path=config_path,
    )
    project_metadata = mocker.MagicMock()
    project_metadata.to_json.return_value = {}
    project_config = mocker.MagicMock()
    project_config.to_CiConfigFromRepo.return_value = None

    handler2.start_scan(project_metadata, project_config)

    assert handler2.scan_id == 42
    assert handler2.deployment_id == 1
    assert handler2.deployment_name == "test-org"
    assert handler2.rules == handler.rules


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
def test_load_scan_config_missing_file_fails(mocker, mock_state, tmp_path):
    """Loading from a nonexistent path logs an error and falls through to the app."""
    mocker.patch("semgrep.app.scans.telemetry")
    handler = ScanHandler(
        enable_transitive_reachability=None,
        load_saved_scan_config_path=tmp_path / "nonexistent.json",
    )
    project_metadata = mocker.MagicMock()
    project_metadata.to_json.return_value = {}
    project_config = mocker.MagicMock()
    project_config.to_CiConfigFromRepo.return_value = None

    with pytest.raises(ValueError, match="Saved scan config not found"):
        handler.start_scan(project_metadata, project_config)


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
def test_no_dump_when_path_not_set(mock_state, tmp_path):
    """No config file is written when dump_scan_config_path is not provided."""
    handler = ScanHandler(
        enable_transitive_reachability=None,
    )
    scan_response = out.ScanResponse.from_json(V1_SCAN_RESPONSE)
    handler._handle_scan_response(scan_response)

    # No json files should have been created in tmp_path
    assert list(tmp_path.glob("*.json")) == []
