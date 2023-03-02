import pytest

from semgrep.app.scans import ScanHandler
from semgrep.engine import EngineType
from semgrep.meta import GitMeta


# Alias the function that's being called to make the test lines shorter
def decide_type(a, b, c):
    return EngineType.decide_engine_type(a, b, c)


@pytest.mark.quick
def test_code_hash_independent_of_filepath(mocker):
    mocker.patch.object(ScanHandler, "deepsemgrep")
    mocker.patch.object(GitMeta, "is_full_scan")
    msh = ScanHandler(False)
    mgm = GitMeta()

    # semgrep scan
    assert decide_type(None, None, None) == EngineType.OSS
    assert decide_type(EngineType.OSS, None, None) == EngineType.OSS
    assert decide_type(EngineType.PRO_LANG, None, None) == EngineType.PRO_LANG
    assert decide_type(EngineType.PRO_INTRAFILE, None, None) == EngineType.PRO_INTRAFILE
    assert decide_type(EngineType.PRO_INTERFILE, None, None) == EngineType.PRO_INTERFILE

    # semgrep ci with toggle on, full scan
    msh.deepsemgrep = True
    mgm.is_full_scan = True
    assert decide_type(None, msh, mgm) == EngineType.PRO_INTERFILE
    assert decide_type(EngineType.OSS, msh, mgm) == EngineType.OSS
    assert decide_type(EngineType.PRO_LANG, msh, mgm) == EngineType.PRO_LANG
    assert decide_type(EngineType.PRO_INTRAFILE, msh, mgm) == EngineType.PRO_INTRAFILE
    assert decide_type(EngineType.PRO_INTERFILE, msh, mgm) == EngineType.PRO_INTERFILE

    # semgrep ci with toggle on, diff scan
    msh.deepsemgrep = True
    mgm.is_full_scan = False
    assert decide_type(None, msh, mgm) == EngineType.PRO_LANG
    assert decide_type(EngineType.OSS, msh, mgm) == EngineType.OSS
    assert decide_type(EngineType.PRO_LANG, msh, mgm) == EngineType.PRO_LANG
    assert decide_type(EngineType.PRO_INTRAFILE, msh, mgm) == EngineType.PRO_INTRAFILE
    assert decide_type(EngineType.PRO_INTERFILE, msh, mgm) == EngineType.PRO_LANG

    # semgrep ci with toggle off, full scan
    msh.deepsemgrep = False
    mgm.is_full_scan = True
    assert decide_type(None, msh, mgm) == EngineType.OSS
    assert decide_type(EngineType.OSS, msh, mgm) == EngineType.OSS
    assert decide_type(EngineType.PRO_LANG, msh, mgm) == EngineType.PRO_LANG
    assert decide_type(EngineType.PRO_INTRAFILE, msh, mgm) == EngineType.PRO_INTRAFILE
    assert decide_type(EngineType.PRO_INTERFILE, msh, mgm) == EngineType.PRO_INTERFILE

    # semgrep ci with toggle off, diff scan
    msh.deepsemgrep = False
    mgm.is_full_scan = False
    assert decide_type(None, msh, mgm) == EngineType.OSS
    assert decide_type(EngineType.OSS, msh, mgm) == EngineType.OSS
    assert decide_type(EngineType.PRO_LANG, msh, mgm) == EngineType.PRO_LANG
    assert decide_type(EngineType.PRO_INTRAFILE, msh, mgm) == EngineType.PRO_INTRAFILE
    assert decide_type(EngineType.PRO_INTERFILE, msh, mgm) == EngineType.PRO_LANG

    """
     requested_engine: Optional["EngineType"] = None,
     scan_handler: Optional[ScanHandler] = None,
     git_meta: Optional[GitMeta] = None,
    """
