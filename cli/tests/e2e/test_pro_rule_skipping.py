import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.osempass
@pytest.mark.kinda_slow
def test_pro_rule_skipping(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pro-rule-skipping.yaml", target_name="pro-rule-skipping/x.cls"
        ).stdout,
        "results.json",
    )
