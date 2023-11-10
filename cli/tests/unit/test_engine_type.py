import pytest

from semgrep.app.scans import ScanHandler
from semgrep.engine import EngineType as ET
from semgrep.meta import GitMeta


@pytest.mark.quick
@pytest.mark.parametrize(
    ("is_cloud_flag_on", "is_ci_scan_full", "enable_pro_diff", "requested", "expected"),
    [
        # semgrep scan
        (False, None, False, None, ET.OSS),
        (False, None, False, ET.OSS, ET.OSS),
        (False, None, False, ET.PRO_LANG, ET.PRO_LANG),
        (False, None, False, ET.PRO_INTRAFILE, ET.PRO_INTRAFILE),
        (False, None, False, ET.PRO_INTERFILE, ET.PRO_INTERFILE),
        # semgrep ci with toggle on, full scan
        (True, True, False, None, ET.PRO_INTERFILE),
        (True, True, False, ET.OSS, ET.OSS),
        (True, True, False, ET.PRO_LANG, ET.PRO_LANG),
        (True, True, False, ET.PRO_INTRAFILE, ET.PRO_INTRAFILE),
        (True, True, False, ET.PRO_INTERFILE, ET.PRO_INTERFILE),
        # semgrep ci with toggle on, diff scan
        (True, False, False, None, ET.PRO_INTRAFILE),
        (True, False, False, ET.OSS, ET.OSS),
        (True, False, False, ET.PRO_LANG, ET.PRO_LANG),
        (True, False, False, ET.PRO_INTRAFILE, ET.PRO_INTRAFILE),
        (True, False, False, ET.PRO_INTERFILE, ET.PRO_INTRAFILE),
        (True, False, True, ET.PRO_INTERFILE, ET.PRO_INTERFILE),
        # semgrep ci with toggle off, full scan
        (False, True, False, None, ET.OSS),
        (False, True, False, ET.OSS, ET.OSS),
        (False, True, False, ET.PRO_LANG, ET.PRO_LANG),
        (False, True, False, ET.PRO_INTRAFILE, ET.PRO_INTRAFILE),
        (False, True, False, ET.PRO_INTERFILE, ET.PRO_INTERFILE),
        # semgrep ci with toggle off, diff scan
        (False, False, False, None, ET.OSS),
        (False, False, False, ET.OSS, ET.OSS),
        (False, False, False, ET.PRO_LANG, ET.PRO_LANG),
        (False, False, False, ET.PRO_INTRAFILE, ET.PRO_INTRAFILE),
        (False, False, False, ET.PRO_INTERFILE, ET.PRO_INTRAFILE),
        (False, False, True, ET.PRO_INTERFILE, ET.PRO_INTERFILE),
    ],
)
def test_decide_engine_type(
    mocker, is_cloud_flag_on, is_ci_scan_full, enable_pro_diff, requested, expected
):
    scan_handler = None
    git_meta = None

    if is_ci_scan_full is not None:  # None means we're in `semgrep scan`
        scan_handler = mocker.Mock(spec=ScanHandler)
        scan_handler.deepsemgrep = is_cloud_flag_on

        git_meta = mocker.Mock(spec=GitMeta)
        git_meta.is_full_scan = is_ci_scan_full

    assert (
        ET.decide_engine_type(
            requested_engine=requested,
            scan_handler=scan_handler,
            git_meta=git_meta,
            enable_pro_diff_scan=enable_pro_diff,
        )
        == expected
    )

    # Expect engine to be non-interfile/intrafile pro
    expected_supply_chain_only_engine = expected if expected == ET.OSS else ET.PRO_LANG
    assert (
        ET.decide_engine_type(
            requested_engine=requested,
            scan_handler=scan_handler,
            git_meta=git_meta,
            enable_pro_diff_scan=enable_pro_diff,
            supply_chain_only=True,
        )
        == expected_supply_chain_only_engine
    )
