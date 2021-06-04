import io
from typing import Any
from typing import Dict
from typing import List

import pytest
from ruamel.yaml import YAML

from semgrep.constants import OutputFormat
from semgrep.core_runner import CoreRunner
from semgrep.error import SemgrepError
from semgrep.error import SourceParseError
from semgrep.output import OutputSettings
from semgrep.pattern import Pattern
from semgrep.rule import Rule

yaml = YAML()


def test_different_hash():
    # SemgrepErrors with differing fields have different hash
    error_1 = SourceParseError(
        short_msg="1",
        long_msg="2",
        spans=[],
        help="4",
    )

    error_2 = SourceParseError(
        short_msg="not 1",
        long_msg="2",
        spans=[],
        help="4",
    )

    assert error_1.__hash__() != error_2.__hash__()

    errors = set()
    errors.add(error_1)
    assert error_2 not in errors


def test_same_hash():
    # SemgrepErrors with all same fields have the same hash
    error_1 = SourceParseError(
        short_msg="1",
        long_msg="2",
        spans=[],
        help="4",
    )

    error_2 = SourceParseError(
        short_msg="1",
        long_msg="2",
        spans=[],
        help="4",
    )

    assert error_1.__hash__() == error_2.__hash__()

    errors = set()
    errors.add(error_1)
    assert error_2 in errors


# cf. https://github.com/returntocorp/semgrep/issues/2237
def test_raise_semgrep_error_from_json_unknown_error():
    test_rule_id = "test_rule_id"
    rule_yaml_text = io.StringIO(
        f"""
    rules:
    - id: {test_rule_id}
      pattern: $X == $X
      severity: INFO
      languages: [python]
      message: blah
    """
    )
    rule_dict = yaml.load(rule_yaml_text).get("rules")[0]
    rule: Rule = Rule.from_json(rule_dict)

    core_runner = CoreRunner(
        allow_exec=False,
        output_settings=OutputSettings(OutputFormat.TEXT),
        jobs=1,
        timeout=0,
        max_memory=0,
        timeout_threshold=0,
        optimizations="all",
    )

    patterns: List[Pattern] = list(core_runner._flatten_rule_patterns([rule]))

    output_json: Dict[str, Any] = {
        "error": "unknown exception",
        "message": "End_of_file",
    }
    with pytest.raises(SemgrepError) as excinfo:
        core_runner._raise_semgrep_error_from_json(output_json, patterns, rule)
        assert test_rule_id in str(excinfo.value)
