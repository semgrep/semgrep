import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


@pytest.mark.kinda_slow
@pytest.mark.parametrize("output_format", [OutputFormat.SARIF])
@pytest.mark.parametrize(
    "rule_and_target",
    [("rules/eqeq.yaml", "basic/stupid.py"), ("rules/cwe_tag.yaml", "basic/stupid.py")],
)
def test_sarif_output(run_semgrep_in_tmp: RunSemgrep, output_format, rule_and_target):
    rule, target = rule_and_target
    _out, err = run_semgrep_in_tmp(
        rule,
        target_name=target,
        options=["--verbose", "--use-osemgrep-sarif"],
        output_format=output_format,
        assert_exit_code=0,
    )
    # When --verbose is enabled, we log
    #   "Osemgrep vs Pysemgrep SARIF output mismatch."
    # when osemgrep and pysemgrep outputs don't match.
    #
    # See an example log in osemgrep_sarif.py. For other
    # output formats, we should log this consistently
    # for it to be tested here.
    assert "output mismatch" not in err
