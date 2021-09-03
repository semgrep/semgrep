from unittest import mock

import semgrep.version


def test_version_check_caching(tmp_path):
    tmp_cache_path = tmp_path / "semgrep_version"

    with mock.patch("semgrep.version._fetch_latest_version") as fetched_mock:
        fetched_mock.return_value = '{"version":1.2.3}'
        semgrep.version.version_check(tmp_cache_path)
        semgrep.version.version_check(tmp_cache_path)

    assert fetched_mock.call_count == 1
