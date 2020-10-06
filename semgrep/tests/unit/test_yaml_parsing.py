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
