from textwrap import dedent

import pytest

from semgrep.config_resolver import parse_config_string
from semgrep.rule import Rule


def create_validator_rule(validation_state_action: str) -> Rule:
    config = parse_config_string(
        "testfile",
        dedent(
            f"""
        rules:
        - id: rule_id
          pattern: $X == $X
          languages: [python]
          severity: INFO
          message: bad
          metadata:
            dev.semgrep.validation_state.actions:
              valid: {validation_state_action}
              invalid: comment
              error: disabled
          validators:
          - http:
              request:
                headers:
                  Authorization: Bearer $REGEX
                  Host: api.example.com
                  User-Agent: Semgrep
                method: GET
                url: https://api.example.com/user
              response:
                - match:
                    status-code: "200"
                  result:
                    metadata:
                      confidence: HIGH
                    validity: valid
                - match:
                      status-code: "401"
                  result:
                    metadata:
                      confidence: MEDIUM
                    validity: invalid
        """
        ),
        None,
    )
    return Rule.from_yamltree(config["testfile"].value["rules"].value[0])


@pytest.mark.quick
def test_rule_full_hash_equivalency():
    config = parse_config_string(
        "testfile",
        dedent(
            """
        rules:
        - id: rule1
          pattern: $X == $X
          languages: [python]
          severity: INFO
          message: bad
        - id: rule1
          pattern: $X == $X
          languages: [python]
          severity: INFO
          message: bad
          metadata:
            this: "is the only difference"
        - id: rule1
          pattern: $X == $X
          languages: [python]
          severity: WARNING  # this is different
          message: bad
        """
        ),
        None,
    )
    rule1, rule2, rule3 = (
        Rule.from_yamltree(tree) for tree in config["testfile"].value["rules"].value
    )
    assert rule1.full_hash == rule2.full_hash
    assert rule1.full_hash != rule3.full_hash
    assert rule2.full_hash != rule3.full_hash


@pytest.mark.quick
@pytest.mark.parametrize(("action", "expected"), [("block", True), ("monitor", False)])
def test_validator_rule_is_blocking(action, expected):
    rule = create_validator_rule(action)
    assert rule.is_blocking == expected
