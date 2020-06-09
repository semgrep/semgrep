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
    # basic spans
    assert data.span == Span(
        start=Position(line=1, column=0),
        end=Position(line=9, column=0),
        file=Path("filename"),
    )

    # values act like dictionaries
    assert data.value["a"].span == Span(
        start=Position(line=2, column=2),
        end=Position(line=9, column=0),
        file=Path("filename"),
    )

    # values act like lists
    assert data.value["a"].value[0].span == Span(
        start=Position(line=2, column=4),
        end=Position(line=2, column=5),
        file=Path("filename"),
    )

    assert data.value["a"].value[0].value == 1

    # spans are also attached to keys
    kvs = list(data.value.items())
    key, value = kvs[0]
    assert key.span == Span(
        start=Position(line=1, column=0),
        end=Position(line=1, column=1),
        file=Path("filename"),
    )

    # unrolling is equivalent
    assert data.unroll() == parse_yaml(test_yaml)
