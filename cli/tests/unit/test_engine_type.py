import pytest

from semgrep.app.scans import ScanHandler
from semgrep.engine import EngineType as ET
from semgrep.meta import GitMeta


@pytest.mark.quick
@pytest.mark.parametrize(
    ("is_cloud_flag_on", "is_ci_scan_full", "requested", "expected"),
    [
        # semgrep scan
        (False, None, None, ET.OSS),
        (False, None, ET.OSS, ET.OSS),
        (False, None, ET.PRO_LANG, ET.PRO_LANG),
        (False, None, ET.PRO_INTRAFILE, ET.PRO_INTRAFILE),
        (False, None, ET.PRO_INTERFILE, ET.PRO_INTERFILE),
        # semgrep ci with toggle on, full scan
        (True, True, None, ET.PRO_INTERFILE),
        (True, True, ET.OSS, ET.OSS),
        (True, True, ET.PRO_LANG, ET.PRO_LANG),
        (True, True, ET.PRO_INTRAFILE, ET.PRO_INTRAFILE),
        (True, True, ET.PRO_INTERFILE, ET.PRO_INTERFILE),
        # semgrep ci with toggle on, diff scan
        (True, False, None, ET.PRO_INTRAFILE),
        (True, False, ET.OSS, ET.OSS),
        (True, False, ET.PRO_LANG, ET.PRO_LANG),
        (True, False, ET.PRO_INTRAFILE, ET.PRO_INTRAFILE),
        (True, False, ET.PRO_INTERFILE, ET.PRO_INTRAFILE),
        # semgrep ci with toggle off, full scan
        (False, True, None, ET.OSS),
        (False, True, ET.OSS, ET.OSS),
        (False, True, ET.PRO_LANG, ET.PRO_LANG),
        (False, True, ET.PRO_INTRAFILE, ET.PRO_INTRAFILE),
        (False, True, ET.PRO_INTERFILE, ET.PRO_INTERFILE),
        # semgrep ci with toggle off, full scan
        (False, False, None, ET.OSS),
        (False, False, ET.OSS, ET.OSS),
        (False, False, ET.PRO_LANG, ET.PRO_LANG),
        (False, False, ET.PRO_INTRAFILE, ET.PRO_INTRAFILE),
        (False, False, ET.PRO_INTERFILE, ET.PRO_INTRAFILE),
    ],
)
def test_decide_engine_type(
    mocker, is_cloud_flag_on, is_ci_scan_full, requested, expected
):
    scan_handler = None
    git_meta = None

    if is_ci_scan_full is not None:  # None means we're in `semgrep scan`
        scan_handler = mocker.Mock(spec=ScanHandler)
        scan_handler.deepsemgrep = is_cloud_flag_on

        git_meta = mocker.Mock(spec=GitMeta)
        git_meta.is_full_scan = is_ci_scan_full

    assert ET.decide_engine_type(requested, scan_handler, git_meta) == expected
