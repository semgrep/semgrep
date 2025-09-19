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
