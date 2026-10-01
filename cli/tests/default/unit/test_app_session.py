#
# Copyright (c) 2025 Semgrep Inc.
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
# Testing src/semgrep/app/session.py
import pytest
from requests import Response

from semgrep.app.session import AppSession


@pytest.mark.slow
@pytest.mark.osemfail
def test_app_session_sending_auth_header(monkeypatch, mocker):
    # [send] is a method in the parent class that we do not implement, and is
    # the primative that sends the request
    def fake_send(self, request, **kwargs):
        res = Response()
        res._content = b""
        res.status_code = 200
        res.headers = request.headers
        return res

    monkeypatch.setattr(AppSession, "send", fake_send)

    app_session = AppSession()
    app_session.token = "wibble"

    sample_first_party_urls = [
        "https://semgrep.dev/rules.yaml",
        "https://metrics.semgrep.dev",
        "https://telemetry.semgrep.dev",
        "https://telemetry.dev2.semgrep.dev",
    ]
    sample_third_party_urls = [
        "https://bad.actor.com/rules.yaml",
        "https://mysemgrep.dev/rules.yaml",
    ]

    # The token should be forwarded to first party URLs
    for first_party_url in sample_first_party_urls:
        semgrep_url_headers = app_session.get(first_party_url).headers
        assert semgrep_url_headers["Authorization"] == "Bearer wibble"

    # But should be dropped for non first party URLs
    for third_party_url in sample_third_party_urls:
        bad_url_headers = app_session.get(third_party_url).headers
        assert "Authorization" not in bad_url_headers


@pytest.mark.quick
def test_job_context_scope_and_observations(monkeypatch):
    import json

    from attrs import evolve

    from semgrep.state import get_state

    state = get_state()
    monkeypatch.setattr(
        "semgrep.state.get_state",
        lambda: evolve(
            state,
            env=evolve(
                state.env,
                semgrep_url="https://backend.example",
                job_id="MixedCase-ID",
                job_observations=json.dumps(
                    {
                        "clone_repository": {
                            "started_at": "2026-10-01T12:00:00Z",
                            "completed_at": "2026-10-01T12:01:00Z",
                        }
                    }
                ),
            ),
        ),
    )

    def fake_send(self, request, **kwargs):
        response = Response()
        response.status_code = 200
        response.headers = request.headers
        return response

    monkeypatch.setattr(AppSession, "send", fake_send)
    session = AppSession()
    session.record_job_scan_started()
    started = session.get("https://backend.example/api/cli/v2/scans").headers
    assert started["X-Semgrep-Job-ID"] == "MixedCase-ID"
    observations = json.loads(started["X-Semgrep-Job-Observations"])
    assert observations["clone_repository"]["completed_at"] == "2026-10-01T12:01:00Z"
    assert "started_at" in observations["run_scan"]
    assert "completed_at" not in observations["run_scan"]
    session.record_job_scan_completed()
    completed = session.get("https://backend.example/api/agent/scans/1/results").headers
    assert (
        "completed_at"
        in json.loads(completed["X-Semgrep-Job-Observations"])["run_scan"]
    )
    for url in (
        "https://metrics.semgrep.dev/api/agent/scans/1/results",
        "https://backend.example/rules.yaml",
        "https://backend.example/api/cli/v2/scans-other",
        "http://backend.example/api/cli/v2/scans",
        "https://backend.example:444/api/cli/v2/scans",
    ):
        headers = session.get(url).headers
        assert "X-Semgrep-Job-ID" not in headers
        assert "X-Semgrep-Job-Observations" not in headers


@pytest.mark.quick
@pytest.mark.parametrize(
    "job_id,observations",
    [
        (None, "{}"),
        ("bad\nheader", "{}"),
        ("valid", "not json"),
        ("valid", "[]"),
        ("valid", '{"clone_repository":{"started_at":"invalid"}}'),
    ],
)
def test_optional_job_context_fails_open(monkeypatch, job_id, observations):
    from attrs import evolve

    from semgrep.state import get_state

    state = get_state()
    monkeypatch.setattr(
        "semgrep.state.get_state",
        lambda: evolve(
            state, env=evolve(state.env, job_id=job_id, job_observations=observations)
        ),
    )

    def fake_send(self, request, **kwargs):
        response = Response()
        response.status_code = 200
        response.headers = request.headers
        return response

    monkeypatch.setattr(AppSession, "send", fake_send)
    headers = AppSession().get(state.env.semgrep_url + "/api/cli/v2/scans").headers
    assert headers.get("X-Semgrep-Job-ID") == ("valid" if job_id == "valid" else None)
    assert "X-Semgrep-Job-Observations" not in headers


@pytest.mark.quick
@pytest.mark.parametrize(
    "destination",
    ["https://unrelated.example/api/cli/v2/scans", "https://semgrep.dev/rules.yaml"],
)
def test_redirect_drops_job_context(destination):
    from requests import Request

    session = AppSession()
    original = Request("GET", "https://semgrep.dev/api/cli/v2/scans").prepare()
    redirected = Request(
        "GET",
        destination,
        headers={"X-Semgrep-Job-ID": "private", "X-Semgrep-Job-Observations": "{}"},
    ).prepare()
    response = Response()
    response.request = original
    session.rebuild_auth(redirected, response)
    assert "X-Semgrep-Job-ID" not in redirected.headers
    assert "X-Semgrep-Job-Observations" not in redirected.headers
