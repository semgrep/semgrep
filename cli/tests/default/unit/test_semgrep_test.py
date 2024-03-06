import itertools
from itertools import product
from pathlib import Path
from typing import Any
from typing import Iterable
from typing import Iterator
from typing import Set
from typing import Tuple

import pytest

from semgrep.test import COMMENT_SYNTAXES
from semgrep.test import line_has_ok
from semgrep.test import line_has_rule
from semgrep.test import normalize_rule_ids
from semgrep.test import OK
from semgrep.test import relatively_eq
from semgrep.test import RULEID
from semgrep.test import SPACE_OR_NO_SPACE
from semgrep.test import TODOOK
from semgrep.test import TODORULEID


# cf. https://docs.python.org/3/library/itertools.html#itertools-recipes
@pytest.mark.quick
def powerset(iterable: Iterable) -> Iterable[Tuple[Any, ...]]:
    """powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"""
    s = list(iterable)
    return itertools.chain.from_iterable(
        itertools.combinations(s, r) for r in range(len(s) + 1)
    )


ANNOTATIONS = (TODOOK, TODORULEID, OK, RULEID)
RULE_IDS = ("a", "b", "a.b", "a.b.c")


def _generate_normalize_rule_ids_test_cases() -> Iterator[Tuple[str, Set[str]]]:
    for comment_begin, comment_end in COMMENT_SYNTAXES:
        for annotation in ANNOTATIONS:
            for space in SPACE_OR_NO_SPACE:
                for rule_combo in powerset(RULE_IDS):
                    yield (
                        f"{comment_begin}{space}{annotation}:{space}{(','+space).join(rule_combo)} {comment_end}",
                        set(rule_combo),
                    )


@pytest.mark.quick
@pytest.mark.parametrize(
    "test_case,expected", list(_generate_normalize_rule_ids_test_cases())
)
def test_normalize_rule_ids(test_case, expected):
    assert normalize_rule_ids(test_case) == expected


def _generate_line_has_test_cases(annotation: str) -> Iterator[str]:
    for comment_begin, comment_end in COMMENT_SYNTAXES:
        for space in SPACE_OR_NO_SPACE:
            yield f"{comment_begin}{space}{annotation}:{space}{RULE_IDS[-1]}{comment_end}".strip()


@pytest.mark.quick
@pytest.mark.parametrize(
    "test_case,expected",
    list(product(_generate_line_has_test_cases("ruleid"), (True,)))
    + list(product(_generate_line_has_test_cases("ok"), (False,)))
    + list(product(_generate_line_has_test_cases("something else"), (False,))),
)
def test_line_has_rule(test_case, expected):
    assert line_has_rule(test_case) == expected


@pytest.mark.quick
@pytest.mark.parametrize(
    "test_case,expected",
    list(product(_generate_line_has_test_cases("ruleid"), (False,)))
    + list(product(_generate_line_has_test_cases("ok"), (True,)))
    + list(product(_generate_line_has_test_cases("something else"), (False,))),
)
def test_line_has_ok(test_case, expected):
    assert line_has_ok(test_case) == expected


@pytest.mark.quick
def test_relatively_eq():
    p1 = Path("rules")
    p2 = Path("tests")
    assert relatively_eq(p1, p1 / "my-rule-a.py", p2, p2 / "my-rule-a.yaml") is True
    assert (
        relatively_eq(p1, p1 / "my-rule-a.other.py", p2, p2 / "my-rule-a.yaml") is True
    )
    assert relatively_eq(p1, p1 / "my-rule-b.py", p2, p2 / "my-rule-a.yaml") is False
    assert (
        relatively_eq(p1, p1 / "sub" / "my-rule-a.py", p2, p2 / "my-rule-a.yaml")
        is False
    )
    assert (
        relatively_eq(
            p1, p1 / "sub" / "my-rule-a.py", p2, p2 / "sub" / "my-rule-a.yaml"
        )
        is True
    )
    assert (
        relatively_eq(
            p1, p1 / "javascript" / "my-rule-a.py", p2, p2 / "python" / "my-rule-a.yaml"
        )
        is False
    )
    assert (
        relatively_eq(p1, p1 / "my-rule-a" / "views.py", p2, p2 / "my-rule-a.yaml")
        is True
    )
