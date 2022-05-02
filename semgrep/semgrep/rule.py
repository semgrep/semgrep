import hashlib
import json
from copy import deepcopy
from typing import Any
from typing import cast
from typing import Dict
from typing import List
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Union

import semgrep.output_from_core as core
from semgrep.constants import RuleSeverity
from semgrep.error import InvalidRuleSchemaError
from semgrep.rule_lang import EmptySpan
from semgrep.rule_lang import RuleValidation
from semgrep.rule_lang import Span
from semgrep.rule_lang import YamlMap
from semgrep.rule_lang import YamlTree
from semgrep.semgrep_types import ALLOWED_GLOB_TYPES
from semgrep.semgrep_types import JOIN_MODE
from semgrep.semgrep_types import LANGUAGE
from semgrep.semgrep_types import Language
from semgrep.semgrep_types import SEARCH_MODE


class Rule:
    def __init__(self, raw: YamlTree[YamlMap]) -> None:
        self._yaml = raw
        self._raw: Dict[str, Any] = raw.unroll_dict()

        self._id = str(self._raw["id"])

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
        self._includes = cast(Sequence[str], path_dict.get("include", []))
        self._excludes = cast(Sequence[str], path_dict.get("exclude", []))
        rule_languages: Set[Language] = {
            LANGUAGE.resolve(l, self.languages_span)
            for l in self._raw.get("languages", [])
        }

        # add typescript to languages if the rule supports javascript.
        # TODO: Move this hack to lang.json
        if any(
            language == LANGUAGE.resolve("javascript") for language in rule_languages
        ):
            rule_languages.add(LANGUAGE.resolve("typescript"))
            self._raw["languages"] = sorted(str(l) for l in rule_languages)

        self._languages = sorted(rule_languages)

        # check taint/search mode
        if self._raw.get("mode") == JOIN_MODE:
            self._mode = JOIN_MODE
        else:
            self._mode = SEARCH_MODE

        # TODO: Move this hack to lang.json
        if any(language == LANGUAGE.resolve("regex") for language in self._languages):
            self._validate_none_language_rule()

    def _validate_none_language_rule(self) -> None:
        """
        For regex-only rules, only patterns, pattern-either, and pattern-regex is valid.
        """

        def _recursive_contains(
            obj: Union[Dict[str, Any], List[Any], str], search_key: str
        ) -> bool:
            """
            Returns true if object contains any object that contains search_key as key
            """
            if isinstance(obj, dict):
                for key in obj:
                    if key == search_key:
                        return True

                    if _recursive_contains(obj[key], search_key):
                        return True

            if isinstance(obj, list):
                for elem in obj:
                    if _recursive_contains(elem, search_key):
                        return True

            return False

        if _recursive_contains(self._raw, "pattern"):
            raise InvalidRuleSchemaError(
                short_msg=f"invalid pattern clause",
                long_msg=f"invalid pattern clause 'pattern' with regex-only rules in rule: {self.id}",
                spans=[],
                help=f"use only patterns, pattern-either, pattern-regex, or pattern-not-regex with regex-only rules",
            )

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, type(self)):
            return False

        return self._raw == other._raw

    def __hash__(self) -> int:
        return hash(self.id)

    @property
    def includes(self) -> Sequence[str]:
        return self._includes

    @property
    def excludes(self) -> Sequence[str]:
        return self._excludes

    @property
    def id(self) -> str:  # TODO: return a core.RuleId
        return self._id

    @property
    def id2(self) -> core.RuleId:  # TODO: merge with id
        return core.RuleId(self._id)

    @property
    def message(self) -> str:
        return str(self._raw["message"])

    @property
    def metadata(self) -> Dict[str, Any]:
        return self._raw.get("metadata", {})

    @property
    def is_blocking(self) -> bool:
        """
        Returns if this rule indicates matches should block CI
        """
        return "block" in self.metadata.get("dev.semgrep.actions", ["block"])

    @property
    def severity(self) -> RuleSeverity:
        return RuleSeverity(self._raw["severity"])

    @property
    def mode(self) -> str:
        return self._mode

    @property
    def project_depends_on(self) -> Optional[List[Dict[str, str]]]:
        if "r2c-internal-project-depends-on" in self._raw:
            depends_on = self._raw["r2c-internal-project-depends-on"]
            if "depends-on-either" in depends_on:
                dependencies: List[Dict[str, str]] = depends_on["depends-on-either"]
                return dependencies
            else:
                return [depends_on]
        else:
            return None

    @property
    def languages(self) -> List[Language]:
        return self._languages

    @property
    def languages_span(self) -> Span:
        return self._yaml.value["languages"].span

    @property
    def raw(self) -> Dict[str, Any]:
        return self._raw

    @property
    def fix(self) -> Optional[str]:
        return self._raw.get("fix")

    @property
    def fix_regex(self) -> Optional[Dict[str, Any]]:
        return self._raw.get("fix-regex")

    @classmethod
    def from_json(cls, rule_json: Dict[str, Any]) -> "Rule":
        yaml = YamlTree.wrap(rule_json, EmptySpan)
        return cls(yaml)

    @classmethod
    def from_yamltree(cls, rule_yaml: YamlTree[YamlMap]) -> "Rule":
        return cls(rule_yaml)

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} id={self.id}>"

    def rename_id(self, new_id: str) -> None:
        self._id = new_id
        self._raw["id"] = new_id

    @property
    def full_hash(self) -> str:
        """
        sha256 hash of the whole rule object instead of just the id
        """
        return hashlib.sha256(
            json.dumps(self._raw, sort_keys=True).encode()
        ).hexdigest()

    @property
    def should_run_on_semgrep_core(self) -> bool:
        """
        Used to detect whether the rule had patterns that need to run on the core
        (beyond Python-handled patterns, like `project-depends-on`).
        Remove this code once all rule runnning is done in the core and the answer is always 'yes'
        """

        def has_runnable_rule(d: Dict[str, Any]) -> bool:
            for k in d:
                if k in RuleValidation.PATTERN_KEYS:
                    return True
            return False

        return has_runnable_rule(self._raw)


def rule_without_metadata(rule: Rule) -> Rule:
    rule2 = deepcopy(rule)
    rule2._raw["metadata"] = {}
    return rule2
