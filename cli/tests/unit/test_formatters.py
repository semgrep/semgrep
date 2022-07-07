from io import StringIO

import pytest
from ruamel.yaml import YAML

from semgrep.formatter.sarif import SarifFormatter
from semgrep.rule import Rule
from semgrep.rule_lang import EmptySpan
from semgrep.rule_lang import YamlTree

yaml = YAML(typ="rt")


@pytest.mark.quick
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
    rule = Rule.from_yamltree(YamlTree.wrap(j, EmptySpan))
    tags = SarifFormatter._rule_to_sarif_tags(rule)

    assert all([isinstance(tag, str) for tag in tags])
