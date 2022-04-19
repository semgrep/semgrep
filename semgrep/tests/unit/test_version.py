from unittest import mock

import pytest

from semgrep.app import version


@pytest.mark.quick
def test_version_check_caching(tmp_path):
    tmp_cache_path = tmp_path / "semgrep_version"

    with mock.patch.object(version, "_fetch_latest_version") as fetched_mock:
        fetched_mock.return_value = {"version": "1.2.3"}
        version.version_check(tmp_cache_path)
        version.version_check(tmp_cache_path)

    assert fetched_mock.call_count == 1
