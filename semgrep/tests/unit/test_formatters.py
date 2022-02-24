from io import StringIO

from ruamel.yaml import YAML

from semgrep.formatter.sarif import SarifFormatter
from semgrep.rule import Rule

yaml = YAML(typ="rt")


def test_rule_to_sarif_tags():
    r = """
      id: blah
      languages: [js]
      severity: INFO
      message: blah
      pattern: blah(...)
      metadata:
        cwe:
        - CWE-22
        owasp:
        - A01:2021
    """
    with StringIO(r) as stream:
        j = yaml.load(stream)
    rule = Rule.from_json(j)
    tags = SarifFormatter._rule_to_sarif_tags(rule)

    assert all([isinstance(tag, str) for tag in tags])
