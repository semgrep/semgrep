import pytest
from tests.fixtures import RunSemgrep

# TODO: print message containing the full dotted path to the rule in osemgrep
# See https://github.com/returntocorp/semgrep/pull/8686#issuecomment-1716299070
# @pytest.mark.osempass
@pytest.mark.kinda_slow
def test_pro_rule_skipping(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pro-rule-skipping.yaml", target_name="pro-rule-skipping/x.cls"
        ).stdout,
        "results.json",
    )
