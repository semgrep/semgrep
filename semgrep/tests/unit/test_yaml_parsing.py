from tempfile import NamedTemporaryFile
from textwrap import dedent

from semgrep.config_resolver import Config
from semgrep.config_resolver import parse_config_string
from semgrep.config_resolver import validate_single_rule
from semgrep.constants import RULES_KEY


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
