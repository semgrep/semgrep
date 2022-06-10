from textwrap import dedent

import pytest

from semgrep.config_resolver import parse_config_string
from semgrep.rule import Rule


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
        Rule(tree) for tree in config["testfile"].value["rules"].value
    )
    assert rule1.full_hash == rule2.full_hash
    assert rule1.full_hash != rule3.full_hash
    assert rule2.full_hash != rule3.full_hash
