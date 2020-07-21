from typing import Any
from typing import Dict
from typing import Iterator
from typing import List
from typing import Optional
from typing import Tuple

from semgrep.equivalences import Equivalence
from semgrep.error import InvalidPatternNameError
from semgrep.error import InvalidRuleSchemaError
from semgrep.rule_lang import EmptySpan
from semgrep.rule_lang import Span
from semgrep.rule_lang import YamlMap
from semgrep.rule_lang import YamlTree
from semgrep.semgrep_types import ALLOWED_GLOB_TYPES
from semgrep.semgrep_types import BooleanRuleExpression
from semgrep.semgrep_types import DEFAULT_MODE
from semgrep.semgrep_types import Language
from semgrep.semgrep_types import Mode
from semgrep.semgrep_types import Operator
from semgrep.semgrep_types import OPERATOR_PATTERN_NAMES_MAP
from semgrep.semgrep_types import OPERATORS
from semgrep.semgrep_types import OPERATORS_WITH_CHILDREN
from semgrep.semgrep_types import pattern_names_for_operator
from semgrep.semgrep_types import pattern_names_for_operators
from semgrep.semgrep_types import PatternId
from semgrep.semgrep_types import SEARCH_MODE
from semgrep.semgrep_types import SUPPORTED_MODES
from semgrep.semgrep_types import TAINT_MODE
from semgrep.semgrep_types import YAML_TAINT_MUST_HAVE_KEYS
from semgrep.util import flatten


class Rule:
    def __init__(self, raw: YamlTree[YamlMap]) -> None:
        self._yaml = raw
        self._raw: Dict[str, Any] = raw.unroll_dict()

        # For tracking errors from semgrep-core
        self._pattern_spans: Dict[PatternId, Span] = {}

        paths_tree: Optional[YamlTree] = self._yaml.value.get("paths")
        if paths_tree is None:
            path_dict = {}
        else:
            paths, paths_span = paths_tree.value, paths_tree.span
            if not isinstance(paths, YamlMap):
                path_key = self._yaml.value.key_tree("paths").span
                help_str: Optional[str] = None
                if isinstance(paths, list):
                    help_str = "remove the `-` to convert the list into a mapping"
                raise InvalidRuleSchemaError(
                    short_msg="invalid paths",
                    long_msg=f"the `paths:` targeting rules must be an object with at least one of {ALLOWED_GLOB_TYPES}",
                    spans=[path_key.extend_to(paths_span)],
                    help=help_str,
                )
            for key, value in paths.items():
                if key.value not in ALLOWED_GLOB_TYPES:
                    raise InvalidRuleSchemaError(
                        short_msg="invalid targeting rules",
                        long_msg=f"the `paths:` targeting rules must each be one of {ALLOWED_GLOB_TYPES}",
                        spans=[key.span.with_context(before=1, after=1)],
                    )
                if not isinstance(value.value, list):
                    raise InvalidRuleSchemaError(
                        short_msg="invalid target value",
                        long_msg=f"the `paths:` targeting rule values must be lists",
                        spans=[value.span],
                    )
            path_dict = paths_tree.unroll_dict()
        self._includes = path_dict.get("include", [])
        self._excludes = path_dict.get("exclude", [])
        self._languages = [Language(l) for l in self._raw["languages"]]

        # check taint/search mode
        self._expression, self._mode = self._taint_or_search_patterns_validation(
            self._yaml
        )

    def _taint_or_search_patterns_validation(
        self, rule: YamlTree[YamlMap]
    ) -> Tuple[BooleanRuleExpression, Mode]:

        rule_raw = rule.value
        mode = (
            Mode(str(rule_raw["mode"].unroll()))
            if rule_raw.get("mode")
            else DEFAULT_MODE
        )
        if mode == TAINT_MODE:
            # Raises InvalidRuleSchemaError if fails to parse in search mode
            return self._build_taint_expression(rule), mode
        elif mode == SEARCH_MODE:
            # Raises InvalidRuleSchemaError if fails to parse in search mode
            return self._build_boolean_expression(rule), mode
        else:
            raise InvalidRuleSchemaError(
                short_msg="invalid mode",
                long_msg=f"The only supported modes are {SUPPORTED_MODES}",
                spans=[rule_raw["mode"].span],
            )

    def _parse_boolean_expression(
        self,
        rule_patterns: YamlTree[List[YamlTree]],
        pattern_id_idx: int = 0,
        prefix: str = "",
    ) -> Iterator[BooleanRuleExpression]:
        """
        Move through the expression from the YML, yielding tuples of (operator, unique-id-for-pattern, pattern)
        """
        if not isinstance(rule_patterns.value, list):
            raise InvalidRuleSchemaError(
                short_msg="invalid patterns",
                long_msg=f"invalid type for patterns; expected a list, but found {type(rule_patterns.unroll()).__name__}",
                spans=[rule_patterns.span.with_context(before=1).truncate(5)],
                help=f"perhaps your YAML is missing a `-` on line {rule_patterns.span.start.line}?",
            )
        for rule_index, pattern_tree in enumerate(rule_patterns.value):
            pattern = pattern_tree.value
            if not isinstance(pattern, YamlMap):
                raise InvalidRuleSchemaError(
                    short_msg="invalid pattern",
                    long_msg=f"invalid type for pattern expected dict but found {type(pattern).__name__}",
                    spans=[pattern_tree.span],
                    help=f"Did you mean `pattern: {pattern}`?",
                )
            for boolean_operator_yaml, sub_pattern in pattern.items():
                boolean_operator: str = boolean_operator_yaml.value
                operator = operator_for_pattern_name(boolean_operator_yaml)
                if operator in set(OPERATORS_WITH_CHILDREN):
                    if isinstance(sub_pattern.value, list):
                        sub_expression = self._parse_boolean_expression(
                            sub_pattern, 0, f"{prefix}.{rule_index}.{pattern_id_idx}"
                        )
                        yield BooleanRuleExpression(
                            operator=operator,
                            pattern_id=None,
                            children=list(sub_expression),
                            operand=None,
                        )
                    else:
                        raise InvalidRuleSchemaError(
                            short_msg="missing children",
                            long_msg=f"operator {boolean_operator} must have children",
                            spans=[
                                boolean_operator_yaml.span.extend_to(sub_pattern.span)
                            ],
                        )
                else:
                    pattern_text, pattern_span = sub_pattern.value, sub_pattern.span
                    if isinstance(pattern_text, str):
                        pattern_id = PatternId(f"{prefix}.{pattern_id_idx}")
                        self._pattern_spans[pattern_id] = pattern_span
                        yield BooleanRuleExpression(
                            operator=operator,
                            pattern_id=pattern_id,
                            children=None,
                            operand=pattern_text,
                        )
                        pattern_id_idx += 1
                    else:
                        raise InvalidRuleSchemaError(
                            short_msg="invalid operand",
                            long_msg=f"operand for {boolean_operator} must be a string, but instead was {type(sub_pattern.unroll()).__name__}",
                            spans=[
                                boolean_operator_yaml.span.extend_to(
                                    pattern_span
                                ).truncate(5)
                            ],
                        )

    @staticmethod
    def _validate_operand(operand: YamlTree) -> str:  # type: ignore
        if not isinstance(operand.value, str):
            raise InvalidRuleSchemaError(
                short_msg="invalid operand",
                long_msg=f"type of `pattern` must be a string, but it was a {type(operand.unroll()).__name__}",
                spans=[operand.span.with_context(before=1, after=1)],
            )
        return operand.value

    @staticmethod
    def _validate_list_operand(field: str, operand: YamlTree) -> list:  # type: ignore
        if not isinstance(operand.value, list):
            raise InvalidRuleSchemaError(
                short_msg="invalid operand",
                long_msg=f"type of {field} must be a list, but it was a {type(operand.unroll()).__name__}",
                spans=[operand.span.with_context(before=1, after=1)],
            )
        return operand.value

    def _build_taint_expression(self, rule: YamlTree[YamlMap]) -> BooleanRuleExpression:
        """
        Build an expression from the yml lines in the rule
        """
        rule_raw = rule.value
        _rule_id = rule_raw["id"].unroll()
        if not isinstance(_rule_id, str):
            raise InvalidRuleSchemaError(
                short_msg="invalid id",
                long_msg=f"rule id must be a string, but was {type(_rule_id).__name__}",
                spans=[rule_raw["id"].span],
            )
        if rule_raw.get("metadata"):
            raise InvalidRuleSchemaError(
                short_msg="invalid key",
                long_msg=f"metadata is not supported in {TAINT_MODE} mode",
                spans=[rule_raw.key_tree("metadata").span],
            )
        rule_id = PatternId(_rule_id)
        for pattern_name in YAML_TAINT_MUST_HAVE_KEYS:
            pattern = rule_raw.get(pattern_name)
            if not pattern:
                raise InvalidRuleSchemaError(
                    short_msg=f"missing {pattern_name} key",
                    long_msg=f"In {TAINT_MODE} mode, 'pattern-sources' and 'pattern-sinks' are both required",
                    spans=[rule.span.truncate(10)],
                )
            self._validate_list_operand(pattern_name, pattern)
            self._pattern_spans[rule_id] = pattern.span
        return BooleanRuleExpression(OPERATORS.AND, rule_id, None, None,)

    def _build_boolean_expression(
        self, rule: YamlTree[YamlMap]
    ) -> BooleanRuleExpression:
        """
        Build a boolean expression from the yml lines in the rule
        """
        rule_raw = rule.value
        _rule_id = rule_raw["id"].unroll()
        if not isinstance(_rule_id, str):
            raise InvalidRuleSchemaError(
                short_msg="invalid id",
                long_msg=f"rule id must be a string, but was {type(_rule_id).__name__}",
                spans=[rule_raw["id"].span],
            )
        rule_id = PatternId(_rule_id)
        for pattern_name in pattern_names_for_operator(OPERATORS.AND):
            pattern = rule_raw.get(pattern_name)
            if pattern:
                self._pattern_spans[rule_id] = pattern.span
                return BooleanRuleExpression(
                    OPERATORS.AND, rule_id, None, self._validate_operand(pattern)
                )

        for pattern_name in pattern_names_for_operator(OPERATORS.REGEX):
            pattern = rule_raw.get(pattern_name)
            if pattern:
                self._pattern_spans[rule_id] = pattern.span
                return BooleanRuleExpression(
                    OPERATORS.REGEX, rule_id, None, self._validate_operand(pattern)
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

        required_operator = [
            OPERATORS.AND_ALL,
            OPERATORS.AND_EITHER,
            OPERATORS.REGEX,
            OPERATORS.AND,
        ]

        raise InvalidRuleSchemaError(
            short_msg="missing key",
            long_msg=f"missing a pattern type in rule. Expected one of {pattern_names_for_operators(required_operator)} or both of {sorted(YAML_TAINT_MUST_HAVE_KEYS)} if {TAINT_MODE} mode is specified",
            spans=[rule.span.truncate(10)],
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
    def mode(self) -> str:
        return self._mode

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
    def languages(self) -> List[Language]:
        return self._languages

    @property
    def languages_span(self) -> Span:
        return self._yaml.value["languages"].span

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
    def fix_regex(self) -> Optional[Dict[str, Any]]:  # type: ignore
        return self._raw.get("fix-regex")

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
    def from_yamltree(cls, rule_yaml: YamlTree[YamlMap]) -> "Rule":
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
        new_yaml = YamlTree(
            value=YamlMap(dict(self._yaml.value._internal)), span=self._yaml.span
        )
        new_yaml.value[self._yaml.value.key_tree("id")] = YamlTree(
            value=new_id, span=new_yaml.value["id"].span
        )
        return Rule(new_yaml)

    @property
    def pattern_spans(self) -> Dict[PatternId, Span]:
        return self._pattern_spans


def operator_for_pattern_name(pattern_name: YamlTree[str]) -> Operator:
    for op, pattern_names in OPERATOR_PATTERN_NAMES_MAP.items():
        if pattern_name.value in pattern_names:
            return op

    valid_pattern_names: List[str] = list(
        sorted(flatten(OPERATOR_PATTERN_NAMES_MAP.values()))
    )
    raise InvalidPatternNameError(
        short_msg="invalid pattern name",
        long_msg=f"invalid pattern name: {pattern_name.value}",
        help=f"valid pattern names are {valid_pattern_names}",
        spans=[pattern_name.span.with_context(before=1, after=1)],
    )
