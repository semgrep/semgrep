from typing import Any
from typing import Dict
from typing import Iterator
from typing import List
from typing import Optional

from semgrep.equivalences import Equivalence
from semgrep.error import InvalidRuleSchemaError
from semgrep.rule_lang import EmptySpan
from semgrep.rule_lang import YamlTree
from semgrep.semgrep_types import ALLOWED_GLOB_TYPES
from semgrep.semgrep_types import BooleanRuleExpression
from semgrep.semgrep_types import operator_for_pattern_name
from semgrep.semgrep_types import OPERATORS
from semgrep.semgrep_types import OPERATORS_WITH_CHILDREN
from semgrep.semgrep_types import pattern_names_for_operator
from semgrep.semgrep_types import pattern_names_for_operators
from semgrep.semgrep_types import PatternId
from semgrep.semgrep_types import YAML_VALID_TOP_LEVEL_OPERATORS


class Rule:
    def __init__(self, raw: YamlTree) -> None:
        self._yaml = raw
        self._raw: Dict[str, Any] = raw.unroll()  # type: ignore

        paths = self._raw.get("paths", {})
        if not isinstance(paths, dict):
            raise InvalidRuleSchemaError(
                f"the `paths:` targeting rules must be an object with at least one of {ALLOWED_GLOB_TYPES}"
            )
        for key, value in paths.items():
            if key not in ALLOWED_GLOB_TYPES:
                raise InvalidRuleSchemaError(
                    f"the `paths:` targeting rules must each be one of {ALLOWED_GLOB_TYPES}"
                )
            if not isinstance(value, list):
                raise InvalidRuleSchemaError(
                    f"the `paths:` targeting rule values must be lists"
                )

        self._includes = paths.get("include", [])
        self._excludes = paths.get("exclude", [])

        self._expression = self._build_boolean_expression(self._raw)

    def _parse_boolean_expression(
        self, rule_patterns: List[Dict[str, Any]], pattern_id: int = 0, prefix: str = ""
    ) -> Iterator[BooleanRuleExpression]:
        """
        Move through the expression from the YML, yielding tuples of (operator, unique-id-for-pattern, pattern)
        """
        if not isinstance(rule_patterns, list):
            raise InvalidRuleSchemaError(
                f"invalid type for patterns in rule: {type(rule_patterns)} is not a list; perhaps your YAML is missing a `-` before {rule_patterns}?"
            )
        for rule_index, pattern in enumerate(rule_patterns):
            if not isinstance(pattern, dict):
                raise InvalidRuleSchemaError(
                    f"invalid type for pattern {pattern}: {type(pattern)} is not a dict"
                )
            for boolean_operator, pattern_text in pattern.items():
                operator = operator_for_pattern_name(boolean_operator)
                if operator in set(OPERATORS_WITH_CHILDREN):
                    if isinstance(pattern_text, list):
                        sub_expression = self._parse_boolean_expression(
                            pattern_text, 0, f"{prefix}.{rule_index}.{pattern_id}"
                        )
                        yield BooleanRuleExpression(
                            operator=operator,
                            pattern_id=None,
                            children=list(sub_expression),
                            operand=None,
                        )
                    else:
                        raise InvalidRuleSchemaError(
                            f"operator {boolean_operator} must have children"
                        )
                else:
                    if isinstance(pattern_text, str):
                        yield BooleanRuleExpression(
                            operator=operator,
                            pattern_id=PatternId(f"{prefix}.{pattern_id}"),
                            children=None,
                            operand=pattern_text,
                        )
                        pattern_id += 1
                    else:
                        raise InvalidRuleSchemaError(
                            f"operand {boolean_operator} must be a string, but instead was {type(pattern_text).__name__}"
                        )

    @staticmethod
    def _validate_operand(operand: Any) -> str:  # type: ignore
        if not isinstance(operand, str):
            raise InvalidRuleSchemaError(
                f"type of `pattern` must be a string, but it was a {type(operand).__name__}"
            )
        return operand

    def _build_boolean_expression(
        self, rule_raw: Dict[str, Any]
    ) -> BooleanRuleExpression:
        """
        Build a boolean expression from the yml lines in the rule
        """
        for pattern_name in pattern_names_for_operator(OPERATORS.AND):
            pattern = rule_raw.get(pattern_name)
            if pattern:
                return BooleanRuleExpression(
                    OPERATORS.AND, rule_raw["id"], None, self._validate_operand(pattern)
                )

        for pattern_name in pattern_names_for_operator(OPERATORS.REGEX):
            pattern = rule_raw.get(pattern_name)
            if pattern:
                return BooleanRuleExpression(
                    OPERATORS.REGEX,
                    rule_raw["id"],
                    None,
                    self._validate_operand(pattern),
                )

        for pattern_name in pattern_names_for_operator(OPERATORS.AND_ALL):
            patterns = rule_raw.get(pattern_name)
            if patterns:
                return BooleanRuleExpression(
                    operator=OPERATORS.AND_ALL,
                    pattern_id=None,
                    children=list(self._parse_boolean_expression(patterns)),
                    operand=None,
                )

        for pattern_name in pattern_names_for_operator(OPERATORS.AND_EITHER):
            patterns = rule_raw.get(pattern_name)
            if patterns:
                return BooleanRuleExpression(
                    operator=OPERATORS.AND_EITHER,
                    pattern_id=None,
                    children=list(self._parse_boolean_expression(patterns)),
                    operand=None,
                )

        valid_top_level_keys = list(YAML_VALID_TOP_LEVEL_OPERATORS)
        raise InvalidRuleSchemaError(
            f"missing a pattern type in rule, expected one of {pattern_names_for_operators(valid_top_level_keys)}"
        )

    @property
    def includes(self) -> List[str]:
        return self._includes  # type: ignore

    @property
    def excludes(self) -> List[str]:
        return self._excludes  # type: ignore

    @property
    def id(self) -> str:
        return str(self._raw["id"])

    @property
    def message(self) -> str:
        return str(self._raw["message"])

    @property
    def metadata(self) -> Dict[str, Any]:  # type: ignore
        return self._raw.get("metadata", {})

    @property
    def severity(self) -> str:
        return str(self._raw["severity"])

    @property
    def sarif_severity(self) -> str:
        """
        SARIF v2.1.0-compliant severity string.

        See https://github.com/oasis-tcs/sarif-spec/blob/a6473580/Schemata/sarif-schema-2.1.0.json#L1566
        """
        mapping = {"INFO": "note", "ERROR": "error", "WARNING": "warning"}
        return mapping[self.severity]

    @property
    def sarif_tags(self) -> Iterator[str]:
        """
        Tags to display on SARIF-compliant UIs, such as GitHub security scans.
        """
        if "cwe" in self.metadata:
            yield "cwe"
        if "owasp" in self.metadata:
            yield "owasp"

    @property
    def languages(self) -> List[str]:
        languages: List[str] = self._raw["languages"]
        return languages

    @property
    def raw(self) -> Dict[str, Any]:  # type: ignore
        return self._raw

    @property
    def expression(self) -> BooleanRuleExpression:
        return self._expression

    @property
    def fix(self) -> Optional[str]:
        return self._raw.get("fix")

    @property
    def equivalences(self) -> List[Equivalence]:
        # Use 'i' to make equivalence id's unique
        return [
            Equivalence(f"{self.id}-{i}", eq["equivalence"], self.languages)
            for i, eq in enumerate(self._raw.get(OPERATORS.EQUIVALENCES, []))
        ]

    @classmethod
    def from_json(cls, rule_json: Dict[str, Any]) -> "Rule":  # type: ignore
        yaml = YamlTree.wrap(rule_json, EmptySpan)
        return cls(yaml)

    @classmethod
    def from_yamltree(cls, rule_yaml: YamlTree) -> "Rule":
        return cls(rule_yaml)

    def to_json(self) -> Dict[str, Any]:
        return self._raw

    def to_sarif(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "name": self.id,
            "shortDescription": {"text": self.message},
            "fullDescription": {"text": self.message},
            "defaultConfiguration": {"level": self.sarif_severity},
            "properties": {"precision": "very-high", "tags": list(self.sarif_tags)},
        }

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} id={self.id}>"

    def with_id(self, new_id: str) -> "Rule":
        new_yaml = YamlTree(value=dict(self._yaml.value), span=self._yaml.span)  # type: ignore
        new_yaml.value["id"] = YamlTree(value=new_id, span=new_yaml.value["id"].span)  # type: ignore
        return Rule(new_yaml)
