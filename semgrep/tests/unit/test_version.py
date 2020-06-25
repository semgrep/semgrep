import time

from semgrep.version import is_running_latest


def test_version_check_caching(tmp_path):
    tmp_cache_path = tmp_path / "semgrep_version"

    time1 = time.time()
    is_running_latest(tmp_cache_path)
    time1 = time.time() - time1

    time2 = time.time()
    is_running_latest(tmp_cache_path)
    time2 = time.time() - time2

    assert time2 < time1
