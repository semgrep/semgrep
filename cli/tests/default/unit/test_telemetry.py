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
# Testing src/semgrep/telemetry.py
import pytest

from semgrep.telemetry import _env_from_app_url


class TestEnvFromAppUrl:
    @pytest.mark.quick
    @pytest.mark.parametrize(
        "url, expected",
        [
            # prod: bare domain
            ("https://semgrep.dev", "prod"),
            # prod: customer subdomain
            ("https://intuit.semgrep.dev", "prod"),
            # prod: trailing slash stripped
            ("https://semgrep.dev/", "prod"),
            # staging: exact
            ("https://staging.semgrep.dev", "staging"),
            # staging: subdomain
            ("https://mystack.staging.semgrep.dev", "staging"),
            # dev2: exact
            ("https://dev2.semgrep.dev", "dev2"),
            # dev2: subdomain
            ("https://mystack.dev2.semgrep.dev", "dev2"),
            # dev2: with path
            ("https://dev2.semgrep.dev/some/path", "dev2"),
            # unknown: unrelated domain
            ("https://example.com", "unknown"),
            # unknown: looks similar but isn't
            ("https://notsemgrep.dev", "unknown"),
            ("https://semgrep.dev.evil.com", "unknown"),
        ],
    )
    def test_env_from_app_url(self, url: str, expected: str) -> None:
        assert _env_from_app_url(url) == expected

    @pytest.mark.quick
    def test_invalid_url(self) -> None:
        assert _env_from_app_url("not a url at all ://???") == "unknown"
