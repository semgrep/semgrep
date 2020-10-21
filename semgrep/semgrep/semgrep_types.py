from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Mapping
from typing import NamedTuple
from typing import NewType
from typing import Optional
from typing import Set
from typing import Union

import attr


Language = NewType("Language", str)
Mode = NewType("Mode", str)
PatternId = NewType("PatternId", str)
Operator = NewType("Operator", str)

TAINT_MODE = Mode("taint")
SEARCH_MODE = DEFAULT_MODE = Mode("search")
SUPPORTED_MODES = {TAINT_MODE, SEARCH_MODE}

YAML_TAINT_MUST_HAVE_KEYS = {"pattern-sinks", "pattern-sources"}

NONE_LANGUAGE = Language("none")
GENERIC_LANGUAGE = Language("generic")


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
    operand: Optional[Union[str, Dict]] = None


def pattern_name_for_operator(operator: Operator) -> str:
    # TODO
    return pattern_names_for_operator(operator)[0]


def pattern_names_for_operator(operator: Operator) -> List[str]:
    return OPERATOR_PATTERN_NAMES_MAP[operator]


def pattern_names_for_operators(operators: List[Operator]) -> List[str]:
    return sum((pattern_names_for_operator(op) for op in operators), [])


class RuleGlobs(NamedTuple):
    include: Set[str]
    exclude: Set[str]

    @staticmethod
    def globs_match_path(globs: Set[str], path: Path) -> bool:
        """Return true if at least one of ``globs`` match for given path."""
        subpaths = [path, *path.parents]
        return any(path.match(glob) for path in subpaths for glob in globs)

    def match_path(self, path: Path) -> bool:
        """Whether the rule should result in findings on the given path."""
        if self.globs_match_path(self.exclude, path):
            return False  # path is excluded

        if self.include and not self.globs_match_path(self.include, path):
            return False  # there are includes and this isn't one of them

        return True


ALLOWED_GLOB_TYPES = ("include", "exclude")


class Range(NamedTuple):
    start: int
    end: int
    vars: Mapping[str, Any]

    def is_enclosing_or_eq(self, other_range: "Range") -> bool:
        return (
            self.start <= other_range.start
            and other_range.end <= self.end
            and self.vars_match(other_range)
        )

    def vars_match(self, rhs: "Range") -> bool:
        """
        Returns true if and only if all metavariables in both this and the other Range refer to the same
        variables (if variable nodes), in the same scope, or the same expressions (if expression nodes).

        That is, if two patterns define a "$X", and $X refers to a variable in one pattern, then
        $X must refer to the same variable in both patterns
        :param rhs: The other Range
        """
        to_match = set(self.vars.keys()).intersection(rhs.vars.keys())
        return all(
            self.vars[v] and rhs.vars[v] and self.vars[v] == rhs.vars[v]
            for v in to_match
        )

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} start={self.start} end={self.end} vars={self.vars}>"

    def __hash__(self) -> int:
        return hash((self.start, self.end))

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, type(self)):
            return False

        return (
            self.start == other.start
            and self.end == other.end
            and self.vars_match(other)
        )
