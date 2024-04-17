import pytest

from semgrep.app.scans import ScanHandler
from semgrep.engine import EngineType as ET
from semgrep.error import SemgrepError
from semgrep.meta import GitMeta


@pytest.mark.quick
@pytest.mark.parametrize("is_supply_chain_only", [True, False])
@pytest.mark.parametrize("is_secrets_scan", [True, False])
@pytest.mark.parametrize(
    "engine_flag", [None, ET.OSS, ET.PRO_LANG, ET.PRO_INTRAFILE, ET.PRO_INTERFILE]
)
@pytest.mark.parametrize(
    (
        "logged_in",
        "is_interfile_flag_on",
        "is_git_full_scan",
        "interfile_diff_scan_enabled",
        "expected_default",
    ),
    [
        # These combinations define the expected engine for Code scans when no
        # engine is explicitly requested via a CLI argument. Requesting the engine
        # in the CLI or running Secrets or Supply Chain may change the engine used.
        #
        # `is_interfile_flag_on` is None for non-ci scans
        # `is_git_full_scan` is None for scans without git metadata
        #
        # We assume some invariants and thus don't test them:
        # - not logged_in -> is_interfile_flag_on is None, is_git_full_scan is None
        # - is_interfile_flag_on is None -> is_git_full_scan is None
        #
        # semgrep scan
        (False, None, None, False, ET.OSS),
        (False, None, None, True, ET.OSS),
        (True, None, None, False, ET.OSS),
        (True, None, None, True, ET.OSS),
        # semgrep ci, not logged in
        (False, None, False, False, ET.OSS),
        (False, None, False, True, ET.OSS),
        (False, None, True, False, ET.OSS),
        (False, None, True, True, ET.OSS),
        # semgrep ci with toggle on, full scan
        (True, True, None, False, ET.PRO_INTERFILE),
        (True, True, None, True, ET.PRO_INTERFILE),
        (True, True, True, False, ET.PRO_INTERFILE),
        (True, True, True, True, ET.PRO_INTERFILE),
        # semgrep ci with toggle off, full scan
        (True, False, None, False, ET.PRO_INTRAFILE),
        (True, False, None, True, ET.PRO_INTRAFILE),
        (True, False, True, False, ET.PRO_INTRAFILE),
        (True, False, True, True, ET.PRO_INTRAFILE),
        # semgrep ci with toggle on, diff scan
        (True, True, False, False, ET.PRO_INTRAFILE),
        (True, True, False, True, ET.PRO_INTERFILE),
        # semgrep ci with toggle off, diff scan
        (True, False, False, False, ET.PRO_INTRAFILE),
        (True, False, False, True, ET.PRO_INTRAFILE),
    ],
)
def test_decide_engine_type(
    mocker,
    logged_in,
    is_interfile_flag_on,
    is_git_full_scan,
    interfile_diff_scan_enabled,
    is_supply_chain_only,
    is_secrets_scan,
    engine_flag,
    expected_default,
):
    ci_scan_handler = None
    git_meta = None

    if is_interfile_flag_on is not None:  # None means we're in `semgrep scan`
        ci_scan_handler = mocker.Mock(spec=ScanHandler)
        ci_scan_handler.deepsemgrep = is_interfile_flag_on

    if is_git_full_scan is not None:  # None means there was no metadata
        git_meta = mocker.Mock(spec=GitMeta)
        git_meta.is_full_scan = is_git_full_scan

    args = [
        logged_in,
        engine_flag,
        is_secrets_scan,
        interfile_diff_scan_enabled,
        ci_scan_handler,
        git_meta,
        is_supply_chain_only,
    ]
    if is_secrets_scan and engine_flag is ET.OSS:
        pytest.raises(SemgrepError, ET.decide_engine_type, *args)
    else:
        diff_scan_override = not (
            (is_git_full_scan is None or is_git_full_scan)
            or interfile_diff_scan_enabled
        )
        assert ET.decide_engine_type(*args) == expected_engine_type(
            is_supply_chain_only,
            is_secrets_scan,
            diff_scan_override,
            engine_flag,
            expected_default,
        )


def expected_engine_type(
    is_supply_chain_only,
    is_secrets_scan,
    diff_scan_override,
    engine_flag,
    expected_default,
):
    if engine_flag is None:
        expected = expected_default
    else:
        expected = engine_flag

    # Overrides
    if is_secrets_scan:
        expected = ET.PRO_INTRAFILE if expected is ET.OSS else expected

    if is_supply_chain_only:
        expected = ET.PRO_INTRAFILE if expected is ET.PRO_INTERFILE else expected

    if diff_scan_override and expected is ET.PRO_INTERFILE:
        expected = ET.PRO_INTRAFILE

    return expected
