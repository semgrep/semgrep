from pathlib import Path
from typing import Any
from typing import List
from typing import Mapping
from typing import NamedTuple
from typing import NewType
from typing import Optional
from typing import Set

import attr

PatternId = NewType("PatternId", str)
Operator = NewType("Operator", str)


class OPERATORS:
    AND_ALL: Operator = Operator("and_all")
    AND_NOT: Operator = Operator("and_not")
    AND: Operator = Operator("and")
    AND_EITHER: Operator = Operator("and_either")
    AND_INSIDE: Operator = Operator("and_inside")
    AND_NOT_INSIDE: Operator = Operator("and_not_inside")
    WHERE_PYTHON: Operator = Operator("where_python")
    FIX: Operator = Operator("fix")
    EQUIVALENCES: Operator = Operator("equivalences")
    REGEX: Operator = Operator("regex")


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
    OPERATORS.EQUIVALENCES: ["equivalences"],
    OPERATORS.REGEX: ["pattern-regex"],
}

# These are the only valid top-level keys
YAML_MUST_HAVE_KEYS = {"id", "message", "languages", "severity"}
YAML_OPTIONAL_KEYS = {"metadata", "paths"}
YAML_VALID_TOP_LEVEL_OPERATORS = {
    OPERATORS.AND,
    OPERATORS.AND_ALL,
    OPERATORS.AND_EITHER,
    OPERATORS.FIX,
    OPERATORS.EQUIVALENCES,
    OPERATORS.REGEX,
}
YAML_ALL_VALID_RULE_KEYS = (
    {
        pattern_name
        for op in YAML_VALID_TOP_LEVEL_OPERATORS
        for pattern_name in OPERATOR_PATTERN_NAMES_MAP[op]
    }
    | YAML_MUST_HAVE_KEYS
    | YAML_OPTIONAL_KEYS
)


class InvalidRuleSchema(BaseException):
    pass


@attr.s(auto_attribs=True, frozen=True)
class BooleanRuleExpression:
    operator: Operator
    pattern_id: Optional[PatternId] = None
    # This is a recursive member but mypy is a half-baked dumpster fire.
    # https://github.com/python/mypy/issues/8320
    children: Optional[List[Any]] = None
    operand: Optional[str] = None

    def __attrs_post_init__(self) -> None:
        self._validate()

    def _validate(self) -> None:
        if self.operator in set(OPERATORS_WITH_CHILDREN):
            if self.operand is not None:
                raise InvalidRuleSchema(
                    f"operators `{pattern_names_for_operator(self.operator)}` cannot have operand but found {self.operand}"
                )
        else:
            if self.children is not None:
                raise InvalidRuleSchema(
                    f"only {pattern_names_for_operators(OPERATORS_WITH_CHILDREN)} operators can have children, but found `{pattern_names_for_operator(self.operator)}` with children"
                )

            if self.operand is None:
                raise InvalidRuleSchema(
                    f"operators `{pattern_names_for_operator(self.operator)}` must have operand"
                )
            else:
                if type(self.operand) != str:
                    raise InvalidRuleSchema(
                        f"operand of operators `{pattern_names_for_operator(self.operator)}` must have type string, but is {type(self.operand)}: {self.operand}"
                    )


def operator_for_pattern_name(pattern_name: str) -> Operator:
    for op, pattern_names in OPERATOR_PATTERN_NAMES_MAP.items():
        if pattern_name in pattern_names:
            return op

    valid_pattern_names: List[str] = sum(OPERATOR_PATTERN_NAMES_MAP.values(), [])
    raise NotImplementedError(
        f"invalid pattern name: {pattern_name}, valid pattern names are {valid_pattern_names}"
    )


def pattern_names_for_operator(operator: Operator) -> List[str]:
    return OPERATOR_PATTERN_NAMES_MAP[operator]


def pattern_names_for_operators(operators: List[Operator]) -> List[str]:
    return sum(
        (pattern_names_for_operator(op) for op in OPERATOR_PATTERN_NAMES_MAP), []
    )


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
        return all(self.vars[v] == rhs.vars[v] for v in to_match)

    def __repr__(self) -> str:
        return f"{self.start}-{self.end} {{m: self.vars.get(m, 'None') for m in self.vars}}"

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
