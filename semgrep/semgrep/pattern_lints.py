import itertools
import json
import textwrap
from enum import Enum
from typing import Any
from typing import Dict
from typing import List
from typing import NamedTuple
from typing import Tuple

from glom import glom  # type: ignore
from glom import PathAccessError

from semgrep.dump_ast import parsed_ast
from semgrep.semgrep_types import BooleanRuleExpression
from semgrep.semgrep_types import OPERATORS


class LintError(NamedTuple):
    msg: str


class ParsedPattern(NamedTuple):
    raw: BooleanRuleExpression
    parsed: Dict[str, Any]

    def span_str(self) -> str:
        if self.raw.span:
            return f"(line {self.raw.span.start_line})"
        else:
            return "(source location missing)"


def check_equivalent_patterns(
    pattern_either: BooleanRuleExpression, lang: str = "python"
) -> List[LintError]:
    equivalent_patterns = []

    json_patterns = [
        ParsedPattern(
            raw=p,
            parsed=json.loads(
                parsed_ast(
                    to_json=True, language=lang, pattern=p.operand, targets_str=[]
                )
            ),
        )
        for p in pattern_either.children or []
    ]
    print(f"Comparing {len(json_patterns)} patterns")
    for pair in itertools.combinations(json_patterns, 2):
        patterns = pair
        equivalence = patterns_are_equivalent(
            lang, patterns[0].parsed, patterns[1].parsed
        )
        if equivalence != EquivalentPatterns.Different:
            equivalent_patterns.append((equivalence, patterns))
    return [
        LintError(
            f"These two patterns are equivalent ({equivalence.value}):\n"
            f'Pattern 1: (line {p[0].span_str()})\n{textwrap.indent(p[0].raw.operand or "No pattern", " ")}'
            f'Pattern 2: (line {p[1].span_str()})\n{textwrap.indent(p[1].raw.operand or "No pattern", " ")}'
        )
        for (equivalence, p) in equivalent_patterns
    ]


class EquivalentPatterns(Enum):
    Different = "different"
    ExactMatch = "exact match"
    ReturnPairedWithAssignment = "return paired with assignment"


def patterns_are_equivalent(
    language: str, patt1_json: Dict[str, Any], patt2_json: Dict[str, Any]
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


def lint(rule: BooleanRuleExpression, lang: str = "python") -> List[LintError]:
    lint_results: List[LintError] = []
    linters = LINTS.get(rule.operator, [])
    for linter in linters:
        lint_results += linter(rule, lang)
    for child in rule.children or []:
        lint_results += lint(child, lang)
    return lint_results
