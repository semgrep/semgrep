import io

import pytest
from attr import validate
from tempfile import NamedTemporaryFile
from textwrap import dedent

from ruamel.yaml import YAML
from jsonschema.exceptions import ValidationError

from semgrep.config_resolver import Config
from semgrep.config_resolver import parse_config_string
from semgrep.config_resolver import validate_single_rule
from semgrep.constants import RULES_KEY
from semgrep.error import InvalidRuleSchemaError


def test_parse_taint_rules():
    yaml_contents = """
rules:
  - id: stupid_equal
    pattern: $X == $X
    message: Dude, $X == $X is always true (Unless X is NAN ...)
    languages: [python, javascript]
    severity: WARNING
  - id: stupid_equal2
    mode: search
    pattern: $X == $X
    message: Dude, $X == $X is always true (Unless X is NAN ...)
    languages: [python, javascript]
    severity: WARNING
  - id: example_id
    mode: taint
    pattern-sources:
      - source(...)
      - source1(...)
    pattern-sinks:
      - sink(...)
      - sink1(...)
      - eval(...)
    pattern-sanitizers:
      - sanitize(...)
      - sanitize1(...)
    message: A user input source() went into a dangerous sink()
    languages: [python, javascript]
    severity: WARNING
    """
    yaml = parse_config_string("testfile", yaml_contents, "file.py")
    config = yaml["testfile"].value
    rules = config.get(RULES_KEY)
    for rule_dict in rules.value:
        validate_single_rule("testfile", rule_dict)
    assert True


def test_multiple_configs():
    config1 = dedent(
        """
    rules:
    - id: rule1
      pattern: $X == $X
      languages: [python]
      severity: INFO
      message: bad
    """
    )
    config2 = dedent(
        """
    rules:
    - id: rule2
      pattern: $X == $Y
      languages: [python]
      severity: INFO
      message: good
    - id: rule3
      pattern: $X < $Y
      languages: [c]
      severity: INFO
      message: doog
    """
    )

    with NamedTemporaryFile() as tf1, NamedTemporaryFile() as tf2:
        tf1.write(config1.encode("utf-8"))
        tf2.write(config2.encode("utf-8"))
        tf1.flush()
        tf2.flush()
        config_list = [tf1.name, tf2.name]
        config, errors = Config.from_config_list(config_list)
        assert not errors
        rules = config.get_rules(True)
        assert len(rules) == 3
        assert {"rule1", "rule2", "rule3"} == set([rule.id for rule in rules])


def test_default_yaml_type_safe():
    s = '!!python/object/apply:os.system ["echo Hello world"]'

    # Safe, returns the object
    default_yaml = YAML()
    assert default_yaml.load(io.StringIO(s)) == ["echo Hello world"]

    # Safe, returns the object
    rt_yaml = YAML(typ="rt")
    assert rt_yaml.load(io.StringIO(s)) == ["echo Hello world"]

    # Unsafe, executes the system call
    unsafe_yaml = YAML(typ="unsafe")
    assert unsafe_yaml.load(io.StringIO(s)) == 0


def test_invalid_metavariable_regex():
    rule = dedent(
        """
        rules:
        - id: boto3-internal-network
          patterns:
          - pattern-inside: $MODULE.client(host=$HOST)
          - metavariable-regex:
              metavariable: $HOST
              regex: '192.168\.\d{1,3}\.\d{1,3}'
              metavariable: $MODULE
              regex: (boto|boto3)
          message: "Boto3 connection to internal network"
          languages: [python]
          severity: ERROR
        """
    )

    with pytest.raises(InvalidRuleSchemaError):
        parse_config_string("testfile", rule, None)


def test_invalid_metavariable_comparison():
    rule = dedent(
        """
        rules:
        - id: boto3-internal-network
          patterns:
          - pattern-inside: $MODULE.client(host=$HOST, port=$PORT)
          - metavariable-comparison:
              metavariable: $PORT
              comparison: $PORT > 9999
              metavariable: $MODULE
              comparison: '(server|servers)'
          message: "Boto3 connection to internal network"
          languages: [python]
          severity: ERROR
        """
    )

    with pytest.raises(InvalidRuleSchemaError):
        parse_config_string("testfile", rule, None)


def test_invalid_metavariable_comparison2():
    rule = dedent(
        """
        rules:
        - id: boto3-internal-network
          patterns:
          - pattern-inside: $MODULE.client(host=$HOST, port=$PORT)
          - metavariable-comparison:
              metavariable: $PORT
              comparison: $PORT > 9999
              metavariable: $MODULE
              regex: '(server|servers)'
          message: "Boto3 connection to internal network"
          languages: [python]
          severity: ERROR
        """
    )

    with pytest.raises(InvalidRuleSchemaError):
        parse_config_string("testfile", rule, None)


def test_invalid_pattern_child():
    rule = dedent("""
        rules:
        - id: blah
          message: blah
          severity: INFO
          languages: [python]
          patterns:
          - pattern-either:
            - pattern: $X == $Y
            - pattern-not: $Z == $Z
    """)

    with pytest.raises(InvalidRuleSchemaError):
        parse_config_string("testfile", rule, None)


def test_invalid_rule_with_null():
    rule = dedent("""
        rules:
        - id: blah
          message: ~
          severity: INFO
          languages: [python]
          patterns:
          - pattern-either:
            - pattern: $X == $Y
            - pattern-not: $Z == $Z
    """)

    with pytest.raises(InvalidRuleSchemaError):
        parse_config_string("testfile", rule, None)