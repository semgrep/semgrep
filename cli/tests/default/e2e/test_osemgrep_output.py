from typing import List
from typing import Optional

import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


@pytest.mark.kinda_slow
@pytest.mark.parametrize("output_format", [OutputFormat.SARIF])
@pytest.mark.parametrize(
    "rule_and_target",
    [
        # Simple case that should pass.
        ("rules/eqeq.yaml", "basic/stupid.py"),
        # Whenever there's a CWE tag, there should be a security tag.
        ("rules/cwe_tag.yaml", "basic/stupid.py"),
        # Rules with metavariable-type need parser initialization to parse correctly.
        ("rules/metavariable_type.yaml", "basic/stupid.py"),
        # Dataflow traces in SARIF should abide by --dataflow-traces.
        ("rules/taint_trace.yaml", "taint/taint_trace.cpp"),
    ],
)
@pytest.mark.parametrize("dataflow_traces", [True, False])
def test_sarif_output(
    run_semgrep_in_tmp: RunSemgrep, output_format, rule_and_target, dataflow_traces
):
    rule, target = rule_and_target
    # The type annotation is there to make the type checker happy
    options: Optional[List[str]]
    if dataflow_traces:
        options = ["--verbose", "--use-osemgrep-sarif", "--dataflow-traces"]
    else:
        options = ["--verbose", "--use-osemgrep-sarif"]

    _out, err = run_semgrep_in_tmp(
        rule,
        target_name=target,
        options=options,
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
