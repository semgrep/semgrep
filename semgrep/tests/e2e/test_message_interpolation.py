import pytest


@pytest.mark.parametrize(
    "rule,target",
    [
        (
            "rules/message_interpolation/pattern-inside.yaml",
            "message_interpolation/pattern_inside_basic.py",
        ),
        # This is an example where we do not correctly report $CLASS.
        (
            "rules/message_interpolation/pattern-inside.yaml",
            "message_interpolation/pattern_inside_complex.py",
        ),
        (
            "rules/message_interpolation/pattern-not-inside.yaml",
            "message_interpolation/pattern_not_inside_basic.py",
        ),
        # This case below is an example of our simple message replacement not inferring what the user is after successfully.
        # I, as the author, would actually want to display a message of all classes. However, this is an extreme corner case that
        # we don't need to cover right now.
        (
            "rules/message_interpolation/pattern-not-inside.yaml",
            "message_interpolation/pattern_not_inside_complex.py",
        ),
        (
            "rules/message_interpolation/pattern-either.yaml",
            "message_interpolation/pattern_either_basic.py",
        ),
    ],
)
def test_message_interpolation(run_semgrep_in_tmp, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target),
        "results.json",
    )
