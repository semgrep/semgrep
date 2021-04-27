import functools
from typing import Any
from typing import Dict
from typing import List
from typing import Mapping
from typing import NamedTuple
from typing import NewType
from typing import Optional
from typing import Union

import attr

from semgrep.rule_lang import YamlMap


Language = NewType("Language", str)
Mode = NewType("Mode", str)
PatternId = NewType("PatternId", str)
Operator = NewType("Operator", str)
FileExtension = NewType("FileExtension", str)

TAINT_MODE = Mode("taint")
SEARCH_MODE = DEFAULT_MODE = Mode("search")
SUPPORTED_MODES = {TAINT_MODE, SEARCH_MODE}

YAML_TAINT_MUST_HAVE_KEYS = {"pattern-sinks", "pattern-sources"}


class OPERATORS:
    AND_ALL: Operator = Operator("and_all")
    AND_NOT: Operator = Operator("and_not")
    AND: Operator = Operator("and")
    AND_EITHER: Operator = Operator("and_either")
    AND_INSIDE: Operator = Operator("and_inside")
    AND_NOT_INSIDE: Operator = Operator("and_not_inside")
    WHERE_PYTHON: Operator = Operator("where_python")
    FIX: Operator = Operator("fix")
    FIX_REGEX: Operator = Operator("fix_regex")
    EQUIVALENCES: Operator = Operator("equivalences")
    REGEX: Operator = Operator("regex")
    NOT_REGEX: Operator = Operator("not_regex")
    METAVARIABLE_REGEX: Operator = Operator("metavariable_regex")
    METAVARIABLE_COMPARISON: Operator = Operator("metavariable_comparison")


OPERATORS_WITH_CHILDREN = [OPERATORS.AND_ALL, OPERATORS.AND_EITHER]

OPERATOR_PATTERN_NAMES_MAP = {
    OPERATORS.AND_INSIDE: ["pattern-inside"],
    OPERATORS.AND_NOT_INSIDE: ["pattern-not-inside"],
    OPERATORS.AND_EITHER: ["pattern-either"],
    OPERATORS.AND_NOT: ["pattern-not"],
    OPERATORS.AND: ["pattern"],
    OPERATORS.AND_ALL: ["patterns"],
    OPERATORS.WHERE_PYTHON: ["pattern-where-python"],
    OPERATORS.FIX: ["fix"],
    OPERATORS.FIX_REGEX: ["fix-regex"],
    OPERATORS.EQUIVALENCES: ["equivalences"],
    OPERATORS.REGEX: ["pattern-regex"],
    OPERATORS.NOT_REGEX: ["pattern-not-regex"],
    OPERATORS.METAVARIABLE_REGEX: ["metavariable-regex"],
    OPERATORS.METAVARIABLE_COMPARISON: ["metavariable-comparison"],
}
PATTERN_NAMES_OPERATOR_MAP = {
    v: k for k, vv in OPERATOR_PATTERN_NAMES_MAP.items() for v in vv
}


@attr.s(auto_attribs=True, frozen=True)
class BooleanRuleExpression:
    operator: Operator
    pattern_id: Optional[PatternId] = None
    children: Optional[List["BooleanRuleExpression"]] = None
    operand: Optional[Union[str, YamlMap]] = None


def pattern_name_for_operator(operator: Operator) -> str:
    # TODO
    return pattern_names_for_operator(operator)[0]


def pattern_names_for_operator(operator: Operator) -> List[str]:
    return OPERATOR_PATTERN_NAMES_MAP[operator]


def pattern_names_for_operators(operators: List[Operator]) -> List[str]:
    return sum((pattern_names_for_operator(op) for op in operators), [])


ALLOWED_GLOB_TYPES = ("include", "exclude")


@functools.total_ordering
class Range(NamedTuple):
    start: int
    end: int
    metavariables: Mapping[str, Any]
    propagated_metavariables: Dict[str, Any] = {}

    def is_enclosing_or_eq(self, rhs: "Range") -> bool:
        return self.is_range_enclosing_or_eq(rhs) and self.metavariables_match(rhs)

    def is_range_enclosing_or_eq(self, rhs: "Range") -> bool:
        return self.start <= rhs.start and rhs.end <= self.end

    def metavariables_match(self, rhs: "Range") -> bool:
        """
        Returns true if and only if all metavariables in both this and the other Range
        refer to the same variables (if variable nodes), in the same scope, or the same
        expressions (if expression nodes).

        That is, if two patterns define a "$X", and $X refers to a variable in one
        pattern, then $X must refer to the same variable in both patterns.
        """
        to_match = set(self.metavariables.keys()) & set(rhs.metavariables.keys())
        return all(
            self.metavariables[v]
            and rhs.metavariables[v]
            and self.metavariables[v] == rhs.metavariables[v]
            for v in to_match
        )

    def __repr__(self) -> str:
        return (
            f"<{self.__class__.__name__} "
            f"start={self.start} "
            f"end={self.end} "
            f"metavariables={self.metavariables} "
            f"propagated={self.propagated_metavariables}>"
        )

    def __hash__(self) -> int:
        return hash((self.start, self.end))

    def __eq__(self, rhs: Any) -> bool:
        if not isinstance(rhs, type(self)):
            return False

        return (
            self.start == rhs.start
            and self.end == rhs.end
            and self.metavariables_match(rhs)
        )

    def __lt__(self, rhs: Any) -> bool:
        if not isinstance(rhs, type(self)):
            return False

        diff_self = self.end - self.start
        diff_rhs = rhs.end - rhs.start

        if diff_self == diff_rhs:
            return self.start < rhs.start

        return diff_self < diff_rhs
