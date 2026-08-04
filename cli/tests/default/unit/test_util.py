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
# Testing src/semgrep/util.py
import pytest

from semgrep.util import is_semgrep_url
from semgrep.util import redact_credentials


class TestIsSemgrepUrl:
    """Test cases for the is_semgrep_url function."""

    @pytest.mark.quick
    def test_default_semgrep_dev_urls(self):
        """Test that default semgrep.dev URLs are recognized."""
        assert is_semgrep_url("https://semgrep.dev/rules.yaml") is True
        assert is_semgrep_url("https://semgrep.dev") is True
        assert is_semgrep_url("https://subdomain.semgrep.dev/api") is True
        assert is_semgrep_url("https://metrics.semgrep.dev") is True
        assert is_semgrep_url("https://telemetry.dev2.semgrep.dev") is True
        assert is_semgrep_url("ftp://semgrep.dev") is True

    @pytest.mark.quick
    def test_non_semgrep_urls(self):
        """Test that non-semgrep URLs are not recognized."""
        assert is_semgrep_url("https://bad.actor.com/rules.yaml") is False
        assert is_semgrep_url("https://mysemgrep.dev/rules.yaml") is False
        assert is_semgrep_url("https://github.com/semgrep/semgrep") is False
        assert is_semgrep_url("https://example.com") is False
        assert is_semgrep_url("not-a-url") is False
        assert is_semgrep_url("") is False

    @pytest.mark.quick
    def test_custom_configured_url_respected(self):
        """Test that custom configured URL is respected when passed as parameter."""
        # URLs that don't match fail without a custom url
        assert is_semgrep_url("http://localhost:3000") is False
        assert is_semgrep_url("http://localhost/api") is False

        custom_url = "http://localhost:3000"
        # URLs matching the custom domain should be recognized
        assert is_semgrep_url("http://localhost:3000", custom_url) is True
        assert is_semgrep_url("http://localhost:3000/api", custom_url) is True

        # Default semgrep.dev URLs should still work even with custom URL
        assert is_semgrep_url("https://semgrep.dev/rules.yaml", custom_url) is True
        assert is_semgrep_url("https://subdomain.semgrep.dev/api", custom_url) is True

        # URLs that don't match the custom domain should return False
        assert is_semgrep_url("https://bad.actor.com/rules.yaml", custom_url) is False
        assert is_semgrep_url("https://example.com", custom_url) is False


class TestRedactCredentials:
    @pytest.mark.quick
    def test_url_embedded_credentials(self):
        s = "git fetch https://gitlab-ci-token:glpat-XXXXXXXX@gitlab.example.com/foo.git main"
        out = redact_credentials(s)
        assert "glpat-XXXXXXXX" not in out
        assert "gitlab-ci-token" not in out
        assert "<REDACTED>" in out
        # The non-secret parts of the URL should remain intact for debugging.
        assert "gitlab.example.com/foo.git" in out
        assert "git fetch" in out

    @pytest.mark.quick
    def test_authorization_header(self):
        s = "headers = {'Authorization': 'Bearer abcdef.secret-token-123'}"
        out = redact_credentials(s)
        assert "abcdef.secret-token-123" not in out
        assert "<REDACTED>" in out

    @pytest.mark.quick
    def test_authorization_header_basic(self):
        s = "Authorization: Basic dXNlcjpwYXNz"
        out = redact_credentials(s)
        assert "dXNlcjpwYXNz" not in out
        assert "<REDACTED>" in out

    @pytest.mark.quick
    def test_no_credentials_passthrough(self):
        s = "no secrets here, just a plain message"
        assert redact_credentials(s) == s

    @pytest.mark.quick
    def test_url_without_credentials_passthrough(self):
        s = "fetched from https://example.com/foo"
        assert redact_credentials(s) == s

    @pytest.mark.quick
    def test_multiple_secrets_in_one_string(self):
        s = (
            "git fetch https://user1:secret1@host1/x and "
            "https://user2:secret2@host2/y"
        )
        out = redact_credentials(s)
        assert "secret1" not in out
        assert "secret2" not in out
        assert out.count("<REDACTED>") == 2

    @pytest.mark.quick
    def test_non_https_scheme(self):
        s = "fetched via ftp://user:secret@ftp.example.com/path"
        out = redact_credentials(s)
        assert "secret" not in out
        assert "<REDACTED>" in out
        assert "ftp.example.com/path" in out

    @pytest.mark.quick
    def test_url_with_port_and_query(self):
        s = "git+https://user:t0kEn@host.example.com:8443/path?ref=main"
        out = redact_credentials(s)
        assert "t0kEn" not in out
        assert "<REDACTED>" in out
        # Path, port, and query should be preserved.
        assert "host.example.com:8443/path?ref=main" in out

    @pytest.mark.quick
    def test_bare_token_as_username(self):
        # No `:password` (GitHub PATs, some GitLab configs).
        s = "git fetch https://glpat-XXXX@gitlab.example.com/foo.git"
        out = redact_credentials(s)
        assert "glpat-XXXX" not in out
        assert "<REDACTED>" in out
        assert "gitlab.example.com/foo.git" in out
