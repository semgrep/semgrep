import time
from unittest import mock

import semgrep


def test_version_check_caching(tmp_path):
    tmp_cache_path = tmp_path / "semgrep_version"

    with mock.patch("semgrep.version._fetch_latest_version") as fetched_mock:
        fetched_mock.return_value = "1.2.3"

        time1 = time.time()
        semgrep.version.is_running_latest(tmp_cache_path)
        time1 = time.time() - time1

        time2 = time.time()
        semgrep.version.is_running_latest(tmp_cache_path)
        time2 = time.time() - time2

    assert fetched_mock.call_count == 1
    assert time2 < time1
