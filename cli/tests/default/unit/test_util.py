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
        assert is_semgrep_url("http://localhost/api", custom_url) is True

        # Default semgrep.dev URLs should still work even with custom URL
        assert is_semgrep_url("https://semgrep.dev/rules.yaml", custom_url) is True
        assert is_semgrep_url("https://subdomain.semgrep.dev/api", custom_url) is True

        # URLs that don't match the custom domain should return False
        assert is_semgrep_url("https://bad.actor.com/rules.yaml", custom_url) is False
        assert is_semgrep_url("https://example.com", custom_url) is False
