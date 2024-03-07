import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


@pytest.mark.kinda_slow
@pytest.mark.osemfail
@pytest.mark.parametrize(
    "rule,target",
    [("rules/taint_intrafile.yaml", "taint/taint_intrafile.py")],
)
def test_taint_traces(run_semgrep_in_tmp: RunSemgrep, tmp_path, snapshot, rule, target):
    semgrep_result = run_semgrep_in_tmp(
        rule,
        target_name=target,
        output_format=OutputFormat.TEXT,
        options=["--pro-intrafile"],
    )
    snapshot.assert_match(
        semgrep_result.stdout,
        "results.txt",
    )
