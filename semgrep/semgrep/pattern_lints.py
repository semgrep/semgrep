import itertools
import json
from enum import Enum
from typing import Any
from typing import Dict
from typing import List
from typing import NamedTuple

from glom import glom  # type: ignore
from glom import PathAccessError

from semgrep.dump_ast import parsed_ast
from semgrep.rule_lang import DUMMY_SPAN
from semgrep.rule_lang import RuleLangError
from semgrep.semgrep_types import BooleanRuleExpression
from semgrep.semgrep_types import OPERATORS


class ParsedPattern(NamedTuple):
    raw: BooleanRuleExpression
    parsed: Dict[str, Any]


def pattern_to_json(pattern: str, lang: str) -> Dict[str, Any]:
    # thanks for nothing, mypy
    if not pattern:
        return {}
    obj: Dict[str, Any] = json.loads(
        parsed_ast(to_json=True, language=lang, pattern=pattern, targets=[])
    )
    return obj


def check_equivalent_patterns(
    pattern_either: BooleanRuleExpression, lang: str
) -> List[RuleLangError]:
    equivalent_patterns = []

    # Only test a rule where all the operands are regular strings.
    if [p for p in pattern_either.children or [] if not isinstance(p.operand, str)]:
        return []

    json_patterns = [
        ParsedPattern(raw=p, parsed=pattern_to_json(pattern=p.operand, lang=lang),)
        for p in pattern_either.children or []
    ]
    for pair in itertools.combinations(json_patterns, 2):
        patterns = pair
        equivalence = patterns_are_equivalent(patterns[0].parsed, patterns[1].parsed)
        if equivalence != EquivalentPatterns.Different:
            equivalent_patterns.append((equivalence, patterns))
    return [
        RuleLangError(
            short_msg="redundant patterns",
            long_msg=f"These patterns are redundant ({equivalence.value})",
            level="lint",
            spans=[
                p[0].raw.provenance or DUMMY_SPAN,
                p[1].raw.provenance or DUMMY_SPAN,
            ],
            help="remove one"
            if equivalence == EquivalentPatterns.ExactMatch
            else "remove the first and delete `return` from the second",
        )
        for (equivalence, p) in equivalent_patterns
    ]


class EquivalentPatterns(Enum):
    Different = "different"
    ExactMatch = "exact match"
    ReturnPairedWithAssignment = "return paired with assignment"


def patterns_are_equivalent(
    patt1_json: Dict[str, Any], patt2_json: Dict[str, Any]
) -> EquivalentPatterns:
    if patt1_json == patt2_json:
        return EquivalentPatterns.ExactMatch

    if (patt1_json.get("Ss") is None) or (patt2_json.get("Ss") is None):
        return EquivalentPatterns.Different

    cause = None
    for s1, s2 in zip(patt1_json["Ss"], patt2_json["Ss"]):
        if s1 == s2:
            continue

        statments = [s1, s2]
        ret = [s for s in statments if s.keys() == {"Return"}]
        expr = [s for s in statments if s.keys() == {"ExprStmt"}]
        if ret and expr:
            if assignment_matches_return(expr=expr[0], ret=ret[0]):
                cause = EquivalentPatterns.ReturnPairedWithAssignment
                continue
        return EquivalentPatterns.Different
    assert cause is not None
    return cause


def assignment_matches_return(expr: Dict[str, Any], ret: Dict[str, Any]) -> bool:
    try:
        expr_rhs = glom(expr, "ExprStmt.Assign.2")
        return_rhs = glom(ret, "Return.1.some")
        return expr_rhs == return_rhs  # type: ignore
    except PathAccessError:
        return False


LINTS = {OPERATORS.AND_EITHER: [check_equivalent_patterns]}


def lint(rule: BooleanRuleExpression, lang: str) -> List[RuleLangError]:
    lint_results: List[RuleLangError] = []
    linters = LINTS.get(rule.operator, [])
    for linter in linters:
        lint_results += linter(rule, lang)
    for child in rule.children or []:
        lint_results += lint(child, lang)
    return lint_results
