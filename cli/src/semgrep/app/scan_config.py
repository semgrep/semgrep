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
"""Fetch scan configurations using asynchronous generation, polling, and retries."""
import base64
import json
from dataclasses import dataclass
from datetime import datetime
from datetime import timedelta
from time import sleep
from typing import Optional

import requests

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep import telemetry
from semgrep.error import INVALID_API_KEY_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.state import get_state
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

DEFAULT_POLL_INTERVAL_SECONDS = 5
MINIMUM_POLL_INTERVAL_SECONDS = 1
MAXIMUM_POLL_INTERVAL_SECONDS = 60


@dataclass(frozen=True)
class ScanConfigResponse:
    """Fetched configuration; callers that use engine parameters must require them."""

    info: out.ScanInfo
    config: out.ScanConfiguration
    engine_params: Optional[out.EngineConfiguration]


class _ConfigPollTimeout(Exception):
    """
    Raised when v2 config polling times out for one POST attempt.
    The backend may have silently dropped the scan job; the POST can be retried.

    Hard backend failures (Failure status) are NOT wrapped in this exception —
    they propagate as plain Exception and bypass the POST retry loop entirely.
    """


def _raise_if_request_failed(response: requests.Response) -> None:
    """
    Handle HTTP response errors from the backend.

    :param response: The HTTP response to check
    :raises SemgrepError: On 401 (invalid API key)
    :raises Exception: On 404 or other HTTP errors
    """
    state = get_state()

    if response.status_code == 401:
        raise SemgrepError(
            "Invalid API Key. Run `semgrep logout` and `semgrep login` again. "
            "Or in CI, ensure your SEMGREP_APP_TOKEN variable is set correctly.",
            code=INVALID_API_KEY_EXIT_CODE,
        )

    if response.status_code == 404:
        raise Exception(
            "Failed to create a scan with given token and deployment_id. "
            "Please make sure they have been set correctly. "
            f"API server at {state.env.semgrep_url} returned this response: {response.text}"
        )

    try:
        response.raise_for_status()
    except requests.RequestException:
        raise Exception(
            f"API server at {state.env.semgrep_url} returned this error: {response.text}"
        )


# coupling(backend): if you change this you must change poll_scan_config_v2 in Semgrep_App.ml
@telemetry.trace()
def fetch_scan_config(request: out.CreateScanRequestV2) -> ScanConfigResponse:
    """
    Create a scan using the v2 endpoint with async config generation.

    1. POST to /api/cli/v2/scans to create scan (returns scan info immediately)
    2. Poll GET /api/cli/v2/scans/{scan_request_id}/config for up to
       SEMGREP_V2_POLL_TIMEOUT_SECONDS per attempt
    3. If polling times out (backend likely dropped the job), retry the POST up to
       SEMGREP_V2_POST_MAX_ATTEMPTS times, subject to an overall
       SEMGREP_V2_OVERALL_TIMEOUT_MINUTES cap.
    4. Return config and scan info, with engine parameters if provided.

    Note: scan_request_id is the client-generated unique_id, not the server's scan.id.
    The same scan_request_id is reused across retries because the POST is idempotent.
    """
    state = get_state()
    poll_timeout_seconds = state.env.v2_poll_timeout_seconds
    max_attempts = state.env.v2_post_max_attempts
    overall_timeout_minutes = state.env.v2_overall_timeout_minutes
    span = telemetry.get_current_span()

    # scan_request_id is the client-generated unique ID; stable across retries
    scan_request_id = request.scan_metadata.unique_id.value
    span.set_attribute("scan.v2.scan_request_id", scan_request_id)

    overall_deadline = datetime.now().replace(tzinfo=None) + timedelta(
        minutes=overall_timeout_minutes
    )

    # saved so we can log the last exception after the final attempt times out
    last_timeout_exc: Optional[_ConfigPollTimeout] = None
    job_context = state.env.job_context
    headers = (
        {
            "X-Semgrep-Job-Context": "base64:"
            + base64.b64encode(job_context.encode("utf-8")).decode("ascii")
        }
        if job_context is not None
        else {}
    )
    for post_attempt in range(1, max_attempts + 1):
        span.set_attribute("scan.v2.post_attempt", post_attempt)

        logger.debug(
            f"Starting scan (v2) attempt {post_attempt}/{max_attempts} "
            f"with request_id={scan_request_id}: {json.dumps(request.to_json(), indent=4)}"
        )

        create_response = state.app_session.post(
            f"{state.env.semgrep_url}/api/cli/v2/scans",
            json=request.to_json(),
            headers=headers,
        )
        _raise_if_request_failed(create_response)

        create_scan_response = out.CreateScanResponseV2.from_json(
            create_response.json()
        )
        scan_info = create_scan_response.info

        # Note: scan_info.id can be null for dry runs
        if scan_info.id:
            logger.debug(f"Scan created with ID: {scan_info.id}")
            span.set_attribute("scan.v2.scan_id", scan_info.id)

        remaining_seconds = (
            overall_deadline - datetime.now().replace(tzinfo=None)
        ).total_seconds()
        if remaining_seconds <= 0:
            break

        poll_timeout = min(poll_timeout_seconds, remaining_seconds)
        try:
            return _poll_for_config_v2(scan_request_id, scan_info, poll_timeout)
        except _ConfigPollTimeout as e:
            last_timeout_exc = e
            remaining_seconds = (
                overall_deadline - datetime.now().replace(tzinfo=None)
            ).total_seconds()
            if post_attempt < max_attempts and remaining_seconds > 0:
                logger.warning(
                    f"Config not ready after {poll_timeout:.0f}s "
                    f"(attempt {post_attempt}/{max_attempts}), retrying POST"
                )
            else:
                break  # deadline exceeded or final attempt — don't make another POST

    cause = (
        f": {last_timeout_exc}"
        if last_timeout_exc
        else " (deadline reached before polling started)"
    )
    raise Exception(
        f"Config generation timed out after {post_attempt} POST attempts "
        f"(scan_request_id={scan_request_id}){cause}"
    )


def _poll_for_config_v2(
    scan_request_id: str,
    scan_info: out.ScanInfo,
    timeout_seconds: float,
) -> ScanConfigResponse:
    """
    Poll GET /api/cli/v2/scans/{scan_request_id}/config until Success, Failure,
    or timeout_seconds elapses.

    Raises:
        _ConfigPollTimeout: config is still Pending after timeout (retryable via POST)
        Exception: backend explicitly reported Failure (not retryable)
    """
    state = get_state()
    span = telemetry.get_current_span()

    start_time = datetime.now().replace(tzinfo=None)
    deadline = start_time + timedelta(seconds=timeout_seconds)

    server_poll_interval_seconds = DEFAULT_POLL_INTERVAL_SECONDS

    poll_attempt = 0
    while datetime.now().replace(tzinfo=None) < deadline:
        poll_attempt += 1
        span.set_attribute("scan.v2.poll_attempts", poll_attempt)

        logger.debug("Polling for scan config")

        config_response = state.app_session.get(
            f"{state.env.semgrep_url}/api/cli/v2/scans/{scan_request_id}/config",
            timeout=state.env.upload_findings_timeout,
        )

        _raise_if_request_failed(config_response)

        get_config_response = out.GetConfigResponseV2.from_json(config_response.json())
        status = get_config_response.status

        # Use server's recommended poll interval if provided
        if polling_info := get_config_response.polling:
            server_poll_interval_seconds = polling_info.recommended_wait_seconds

        if isinstance(status.value, out.Success):
            # Config is ready
            span.set_attribute("scan.v2.config_ready", True)

            if get_config_response.config is None:
                raise Exception("Config status is Success but config is missing")

            return ScanConfigResponse(
                info=scan_info,
                config=get_config_response.config,
                engine_params=get_config_response.engine_params,
            )

        elif isinstance(status.value, out.Failure):
            # Config generation failed
            span.set_attribute("scan.v2.config_status", "failure")
            raise Exception(
                f"Config generation failed for scan_request_id={scan_request_id}"
            )

        elif isinstance(status.value, out.Pending):
            # Still pending - continue polling
            span.set_attribute("scan.v2.config_status", "pending")

        # Never wait less than minimum poll interval to avoid hammering the server
        sleep(
            min(
                max(server_poll_interval_seconds, MINIMUM_POLL_INTERVAL_SECONDS),
                MAXIMUM_POLL_INTERVAL_SECONDS,
            )
        )

    # Timeout - config never became ready
    elapsed = (datetime.now().replace(tzinfo=None) - start_time).seconds
    raise _ConfigPollTimeout(
        f"Config still pending after {elapsed}s "
        f"(scan_request_id={scan_request_id}, {poll_attempt} poll attempts)"
    )
