from dataclasses import dataclass
from typing import Dict
from typing import List
from typing import NewType
from typing import Optional


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


OPERATORS_WITH_CHILDREN = [OPERATORS.AND_ALL, OPERATORS.AND_EITHER]

PATTERN_NAMES_MAP = {
    "pattern-inside": OPERATORS.AND_INSIDE,
    "pattern-not-inside": OPERATORS.AND_NOT_INSIDE,
    "pattern-either": OPERATORS.AND_EITHER,
    "pattern-not": OPERATORS.AND_NOT,
    "pattern": OPERATORS.AND,
    "patterns": OPERATORS.AND_ALL,
    "pattern-where-python": OPERATORS.WHERE_PYTHON,
}

INVERSE_PATTERN_NAMES_MAP = dict((v, k) for k, v in PATTERN_NAMES_MAP.items())

# These are the only valid top-level keys
YAML_MUST_HAVE_KEYS = {"id", "message", "languages", "severity"}
YAML_VALID_TOP_LEVEL_OPERATORS = {
    OPERATORS.AND,
    OPERATORS.AND_ALL,
    OPERATORS.AND_EITHER,
}
YAML_ALL_VALID_RULE_KEYS = set(
    [INVERSE_PATTERN_NAMES_MAP[k] for k in YAML_VALID_TOP_LEVEL_OPERATORS]
).union(YAML_MUST_HAVE_KEYS)


class InvalidRuleSchema(BaseException):
    pass


@dataclass(frozen=True)
class BooleanRuleExpression:
    operator: Operator
    pattern_id: Optional[PatternId] = None
    children: Optional[List["BooleanRuleExpression"]] = None
    operand: Optional[str] = None

    def __post_init__(self) -> None:
        self._validate()

    def _validate(self) -> None:
        if self.operator in set(OPERATORS_WITH_CHILDREN):
            if self.operand is not None:
                raise InvalidRuleSchema(
                    f"operator `{pattern_name_for_operator(self.operator)}` cannot have operand but found {self.operand}"
                )
        else:
            if self.children is not None:
                raise InvalidRuleSchema(
                    f"only {list(map(pattern_name_for_operator, OPERATORS_WITH_CHILDREN))} operators can have children, but found `{pattern_name_for_operator(self.operator)}` with children"
                )

            if self.operand is None:
                raise InvalidRuleSchema(
                    f"operator `{pattern_name_for_operator(self.operator)}` must have operand"
                )
            else:
                if type(self.operand) != str:
                    raise InvalidRuleSchema(
                        f"operand of operator `{pattern_name_for_operator(self.operator)}` ought to have type string, but is {type(self.operand)}: {self.operand}"
                    )


def operator_for_pattern_name(pattern_name: str) -> Operator:
    if not pattern_name in PATTERN_NAMES_MAP:
        raise NotImplementedError(
            f"invalid pattern name: {pattern_name}, valid pattern names are {list(PATTERN_NAMES_MAP.keys())}"
        )
    return PATTERN_NAMES_MAP[pattern_name]


def pattern_name_for_operator(operator: Operator) -> str:
    return INVERSE_PATTERN_NAMES_MAP[operator]


@dataclass(frozen=True)
class Range:
    start: int
    end: int

    def is_enclosing_or_eq(self, other_range: "Range") -> bool:
        return self.start <= other_range.start and other_range.end <= self.end

    def __repr__(self) -> str:
        return f"{self.start}-{self.end}"


@dataclass(frozen=True)
class SgrepRange:
    # Wrapper to represent results from sgrep

    range: Range  # The range of the match
    metavars: Dict[str, str]  # Any matched metavariables, {"$NAME": "<matched text>"}

    def __repr__(self) -> str:
        return f"{self.range}-{self.metavars}"
