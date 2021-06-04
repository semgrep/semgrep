import hashlib
import json
from typing import Any
from typing import cast
from typing import Dict
from typing import Iterator
from typing import List
from typing import Optional
from typing import Tuple

from semgrep.equivalences import Equivalence
from semgrep.error import InvalidRuleSchemaError
from semgrep.error import SemgrepError
from semgrep.rule_lang import EmptySpan
from semgrep.rule_lang import Span
from semgrep.rule_lang import YamlMap
from semgrep.rule_lang import YamlTree
from semgrep.semgrep_types import ALLOWED_GLOB_TYPES
from semgrep.semgrep_types import BooleanRuleExpression
from semgrep.semgrep_types import DEFAULT_MODE
from semgrep.semgrep_types import Language
from semgrep.semgrep_types import Language_util
from semgrep.semgrep_types import Mode
from semgrep.semgrep_types import Operator
from semgrep.semgrep_types import OPERATOR_PATTERN_NAMES_MAP
from semgrep.semgrep_types import OPERATORS
from semgrep.semgrep_types import OPERATORS_WITH_CHILDREN
from semgrep.semgrep_types import pattern_names_for_operator
from semgrep.semgrep_types import PATTERN_NAMES_OPERATOR_MAP
from semgrep.semgrep_types import PatternId
from semgrep.semgrep_types import TAINT_MODE
from semgrep.semgrep_types import YAML_TAINT_MUST_HAVE_KEYS


class Rule:
    def __init__(self, raw: YamlTree[YamlMap]) -> None:
        self._yaml = raw
        self._raw: Dict[str, Any] = raw.unroll_dict()

        self._id = str(self._raw["id"])

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
            path_dict = paths_tree.unroll_dict()
        self._includes = path_dict.get("include", [])
        self._excludes = path_dict.get("exclude", [])
        rule_languages = {
            Language_util.resolve(l, self.languages_span)
            for l in self._raw["languages"]
        }

        # add typescript to languages if the rule supports javascript.
        if any(language == Language.JAVASCRIPT for language in rule_languages):
            rule_languages.add(Language.TYPESCRIPT)

        self._languages = sorted(rule_languages, key=lambda lang: lang.value)  # type: ignore

        # check taint/search mode
        self._expression, self._mode = self._build_search_patterns_for_mode(self._yaml)

        if any(language == Language.REGEX for language in self._languages):
            self._validate_none_language_rule()

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, type(self)):
            return False

        return self._raw == other._raw

    def __hash__(self) -> int:
        return hash(self.id)

    def has_pattern_where_python(self) -> bool:
        """
        Return true if this rule contains a pattern-where-python pattern
        """

        def _recursive_dict_key_exists(dictionary: Dict[str, Any], key: str) -> bool:
            """
            Returns true if
            - dictionary contains key KEY or
            - any value in dictionary that is a dict contains the key
            - a value that is a list of dictionaries has a dict that contains the key
            """
            if key in dictionary:
                return True

            for val in dictionary.values():
                if isinstance(val, dict) and _recursive_dict_key_exists(val, key):
                    return True
                if isinstance(val, list):
                    for obj in val:
                        if isinstance(obj, dict) and _recursive_dict_key_exists(
                            obj, key
                        ):
                            return True

            return False

        return _recursive_dict_key_exists(self._raw, "pattern-where-python")

    def _validate_none_language_rule(self) -> None:
        """
        For regex-only rules, only patterns, pattern-either, and pattern-regex is valid.
        """

        def _validate(expression: BooleanRuleExpression, span_key: str = "") -> None:
            """
            Recursively validate expressions
            """
            valid_operators = {
                OPERATORS.REGEX,
                OPERATORS.AND_EITHER,
                OPERATORS.AND_ALL,
                OPERATORS.NOT_REGEX,
            }
            if expression.operator not in valid_operators:
                operator_key = OPERATOR_PATTERN_NAMES_MAP.get(
                    expression.operator, [""]
                )[0]
                doc = self._yaml.value.get(span_key)
                span = doc.span if doc else self._yaml.span
                raise InvalidRuleSchemaError(
                    short_msg=f"invalid pattern clause",
                    long_msg=f"invalid pattern clause '{operator_key}' with regex-only rules",
                    spans=[span],
                    help=f"use only patterns, pattern-either, pattern-regex, or pattern-not-regex with regex-only rules",
                )
            if expression.children:
                for child in expression.children:
                    _validate(child, span_key)

        top = OPERATOR_PATTERN_NAMES_MAP.get(self._expression.operator, [""])[0]
        _validate(self._expression, span_key=top)

    def _build_search_patterns_for_mode(
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
        else:
            # Raises InvalidRuleSchemaError if fails to parse in search mode
            return self._build_boolean_expression(rule), mode

    def _parse_boolean_expression(
        self,
        rule_patterns: YamlTree[List[YamlTree]],
        pattern_id_idx: int = 0,
        prefix: str = "",
    ) -> Iterator[BooleanRuleExpression]:
        """
        Move through the expression from the YML, yielding tuples of (operator, unique-id-for-pattern, pattern)
        """
        for rule_index, pattern_tree in enumerate(rule_patterns.value):
            for boolean_operator_yaml, sub_pattern in pattern_tree.value.items():
                operator = operator_for_pattern_name(boolean_operator_yaml)
                if operator in OPERATORS_WITH_CHILDREN:
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
                    if not isinstance(sub_pattern.value, (str, YamlMap)):
                        raise SemgrepError(
                            f"expected operator '{operator}' to have string or map value guaranteed by schema"
                        )

                    pattern_id = PatternId(f"{prefix}.{pattern_id_idx}")
                    self._pattern_spans[pattern_id] = sub_pattern.span
                    yield BooleanRuleExpression(
                        operator=operator,
                        pattern_id=pattern_id,
                        children=None,
                        operand=sub_pattern.value,
                    )
                    pattern_id_idx += 1

    def _build_taint_expression(self, rule: YamlTree[YamlMap]) -> BooleanRuleExpression:
        """
        Build an expression from the yml lines in the rule
        """
        rule_raw = rule.value
        _rule_id = cast(str, rule_raw["id"].unroll())
        rule_id = PatternId(_rule_id)

        for pattern_name in YAML_TAINT_MUST_HAVE_KEYS:
            pattern = rule_raw[pattern_name]
            self._pattern_spans[rule_id] = pattern.span

        return BooleanRuleExpression(
            OPERATORS.AND,
            rule_id,
            None,
            None,
        )

    def _build_boolean_expression(
        self, rule: YamlTree[YamlMap]
    ) -> BooleanRuleExpression:
        """
        Build a boolean expression from the yml lines in the rule
        """
        rule_raw = rule.value
        _rule_id = cast(str, rule_raw["id"].unroll())
        rule_id = PatternId(_rule_id)

        for pattern_name in pattern_names_for_operator(OPERATORS.AND):
            pattern = rule_raw.get(pattern_name)
            if pattern:
                self._pattern_spans[rule_id] = pattern.span
                return BooleanRuleExpression(
                    OPERATORS.AND, rule_id, None, pattern.value
                )

        for pattern_name in pattern_names_for_operator(OPERATORS.REGEX):
            pattern = rule_raw.get(pattern_name)
            if pattern:
                self._pattern_spans[rule_id] = pattern.span
                return BooleanRuleExpression(
                    OPERATORS.REGEX, rule_id, None, pattern.value
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

        raise SemgrepError(f"rule with id '{_rule_id}' is missing top-level operator")

    @property
    def includes(self) -> List[str]:
        return self._includes  # type: ignore

    @property
    def excludes(self) -> List[str]:
        return self._excludes  # type: ignore

    @property
    def id(self) -> str:
        return self._id

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

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} id={self.id}>"

    def rename_id(self, new_id: str) -> None:
        self._id = new_id

    @property
    def pattern_spans(self) -> Dict[PatternId, Span]:
        return self._pattern_spans

    @property
    def full_hash(self) -> str:
        """
        sha256 hash of the whole rule object instead of just the id
        """
        return hashlib.sha256(
            json.dumps(self._raw, sort_keys=True).encode()
        ).hexdigest()


def operator_for_pattern_name(pattern_name: YamlTree[str]) -> Operator:
    return PATTERN_NAMES_OPERATOR_MAP[pattern_name.value]
