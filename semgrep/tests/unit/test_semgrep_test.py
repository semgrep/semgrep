#!/usr/bin/env python3
from itertools import chain
from itertools import combinations
from itertools import product
from typing import Iterator
from typing import Set
from typing import Tuple

import pytest

from semgrep.test import line_has_ok
from semgrep.test import line_has_rule
from semgrep.test import normalize_rule_ids

# cf. https://docs.python.org/3/library/itertools.html#itertools-recipes
def powerset(iterable):
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s) + 1))


COMMENT_SYNTAXES = (("#", "\n"), ("//", "\n"), ("<!--", "-->"), ("(*", "*)"))
ANNOTATIONS = ("ruleid", "ok")
SPACE_OR_NO_SPACE = ("", " ")
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


@pytest.mark.parametrize(
    "test_case,expected", list(_generate_normalize_rule_ids_test_cases())
)
def test_normalize_rule_ids(test_case, expected):
    assert normalize_rule_ids(test_case) == expected


def _generate_line_has_test_cases(annotation: str) -> Iterator[str]:
    for comment_begin, comment_end in COMMENT_SYNTAXES:
        for space in SPACE_OR_NO_SPACE:
            yield f"{comment_begin}{space}{annotation}:{space}{RULE_IDS[-1]}{comment_end}".strip()


@pytest.mark.parametrize(
    "test_case,expected",
    list(product(_generate_line_has_test_cases("ruleid"), (True,)))
    + list(product(_generate_line_has_test_cases("ok"), (False,)))
    + list(product(_generate_line_has_test_cases("something else"), (False,))),
)
def test_line_has_rule(test_case, expected):
    assert line_has_rule(test_case) == expected


@pytest.mark.parametrize(
    "test_case,expected",
    list(product(_generate_line_has_test_cases("ruleid"), (False,)))
    + list(product(_generate_line_has_test_cases("ok"), (True,)))
    + list(product(_generate_line_has_test_cases("something else"), (False,))),
)
def test_line_has_ok(test_case, expected):
    assert line_has_ok(test_case) == expected
