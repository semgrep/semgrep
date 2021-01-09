import pytest


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
def test_message_interpolation(run_semgrep_in_tmp, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target),
        "results.json",
    )
