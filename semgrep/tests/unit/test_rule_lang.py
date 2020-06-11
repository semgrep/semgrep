from pathlib import Path

from semgrep.rule_lang import parse_yaml
from semgrep.rule_lang import parse_yaml_preserve_spans
from semgrep.rule_lang import Position
from semgrep.rule_lang import Span

test_yaml = """---
a:
  - 1
  - 2
  - "3"
  - key: value
  - pattern: |
        def __eq__():
            ...
"""


def test_span_tracking():
    data = parse_yaml_preserve_spans(test_yaml, Path("filename"))

    def test_span(start: Position, end: Position) -> Span:
        return data.span._replace(start=start, end=end)

    # basic spans
    assert data.span == test_span(
        start=Position(line=2, column=1), end=Position(line=10, column=1),
    )

    # values act like dictionaries
    assert data.value["a"].span == test_span(
        start=Position(line=3, column=3), end=Position(line=10, column=1),
    )

    # values act like lists
    assert data.value["a"].value[1].span == test_span(
        start=Position(line=4, column=5), end=Position(line=4, column=6),
    )

    assert data.value["a"].value[1].value == 2

    # spans are also attached to keys
    kvs = list(data.value.items())
    key, value = kvs[0]
    assert key.span == test_span(
        start=Position(line=2, column=1), end=Position(line=2, column=2),
    )

    # unrolling is equivalent
    assert data.unroll() == parse_yaml(test_yaml)
