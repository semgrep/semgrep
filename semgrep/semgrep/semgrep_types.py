from typing import Any
from typing import List
from typing import NamedTuple
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
    FIX: Operator = Operator("fix")
    EQUIVALENCES: Operator = Operator("equivalences")


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
}

# These are the only valid top-level keys
YAML_MUST_HAVE_KEYS = {"id", "message", "languages", "severity"}
YAML_OPTIONAL_KEYS = {"metadata"}
YAML_VALID_TOP_LEVEL_OPERATORS = {
    OPERATORS.AND,
    OPERATORS.AND_ALL,
    OPERATORS.AND_EITHER,
    OPERATORS.FIX,
    OPERATORS.EQUIVALENCES,
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


class BooleanRuleExpression(NamedTuple):
    operator: Operator
    pattern_id: Optional[PatternId] = None
    # This is a recursive member but mypy is a half-baked dumpster fire.
    # https://github.com/python/mypy/issues/8320
    children: Optional[List[Any]] = None
    operand: Optional[str] = None

    def __post_init__(self) -> None:
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


class Range(NamedTuple):
    start: int
    end: int

    def is_enclosing_or_eq(self, other_range: "Range") -> bool:
        return self.start <= other_range.start and other_range.end <= self.end

    def __repr__(self) -> str:
        return f"{self.start}-{self.end}"
