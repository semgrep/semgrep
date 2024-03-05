import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,target",
    [
        (
            "rules/message_interpolation/pattern-inside.yaml",
            "message_interpolation/pattern_inside_basic.py",
        ),
        (
            "rules/message_interpolation/pattern-inside.yaml",
            "message_interpolation/pattern_inside_complex.py",
        ),
        (
            "rules/message_interpolation/propagated-constant.yaml",
            "message_interpolation/propagated_constant.py",
        ),
        # pattern-not-inside is not currently interpolated. These tests make sure
        # something doesn't break accidentally.
        (
            "rules/message_interpolation/pattern-not-inside.yaml",
            "message_interpolation/pattern_not_inside_basic.py",
        ),
        (
            "rules/message_interpolation/pattern-not-inside.yaml",
            "message_interpolation/pattern_not_inside_complex.py",
        ),
        (
            "rules/message_interpolation/pattern-either.yaml",
            "message_interpolation/pattern_either_basic.py",
        ),
        (
            "rules/message_interpolation/multi-pattern-inside.yaml",
            "message_interpolation/multi_pattern_inside.py",
        ),
        (
            "rules/message_interpolation/multi-pattern-inside.yaml",
            "message_interpolation/multi_pattern_inside_nested.py",
        ),
    ],
)
def test_message_interpolation(run_semgrep_in_tmp: RunSemgrep, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target).stdout, "results.json"
    )


@pytest.mark.slow
def test_no_double_interpolation(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp(
        "rules/message_interpolation/interpolated_message.yaml",
        target_name="message_interpolation/target_with_metavariable.py",
        output_format=OutputFormat.JSON,  # Not the real output format; just disables JSON parsing
    )
    snapshot.assert_match(stdout, "report.json")
