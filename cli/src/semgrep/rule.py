import hashlib
import json
from typing import Any
from typing import AnyStr
from typing import cast
from typing import Dict
from typing import List
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Union

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.constants import RuleScanSource
from semgrep.error import InvalidRuleSchemaError
from semgrep.error_location import Span
from semgrep.rule_lang import EmptySpan
from semgrep.rule_lang import RuleValidation
from semgrep.rule_lang import YamlMap
from semgrep.rule_lang import YamlTree
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_types import JOIN_MODE
from semgrep.semgrep_types import LANGUAGE
from semgrep.semgrep_types import Language
from semgrep.semgrep_types import SEARCH_MODE


class Rule:
    def __init__(
        self, raw: Dict[str, Any], yaml: Optional[YamlTree[YamlMap]] = None
    ) -> None:
        self._raw: Dict[str, Any] = raw
        self._yaml: Optional[YamlTree[YamlMap]] = yaml
        self._id = str(self._raw["id"])

        path_dict = self._raw.get("paths", {})
        self.options_dict = self._raw.get("options", {})
        self._includes = cast(Sequence[str], path_dict.get("include", []))
        self._excludes = cast(Sequence[str], path_dict.get("exclude", []))

        lang_span = (
            yaml.value["languages"].span if yaml and "languages" in yaml.value else None
        )

        def resolve_language_string(language_str: str) -> Language:
            # Replace "generic" in the "languages" list by the engine specified
            # in the options section.
            analyzer_str = (
                self.options_dict.get("generic_engine", "spacegrep")
                if language_str == "generic"
                else language_str
            )
            return LANGUAGE.resolve(analyzer_str, lang_span)

        rule_languages: Set[Language] = {
            resolve_language_string(l) for l in self._raw.get("languages", [])
        }

        # add typescript to languages if the rule supports javascript.
        # TODO: Move this hack to lang.json
        # coupling: if you move this hack, also fix
        # Core_runner.add_typescript_to_javascript_rules_hack
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
    def id(self) -> str:  # TODO: return a out.RuleId
        return self._id

    @property
    def id2(self) -> out.RuleId:  # TODO: merge with id
        return out.RuleId(self._id)

    @property
    def message(self) -> str:
        return str(self._raw.get("message"))

    @property
    def metadata(self) -> Dict[str, Any]:
        return self._raw.get("metadata") or {}

    @property
    def is_blocking(self) -> bool:
        """
        Returns if this rule indicates matches should block CI
        """

        validation_state_metadata = self.metadata.get(
            "dev.semgrep.validation_state.actions"
        )
        if validation_state_metadata:
            return "block" in validation_state_metadata.values()

        return "block" in self.metadata.get("dev.semgrep.actions", ["block"])

    @property
    def severity(self) -> out.MatchSeverity:
        # TODO: add additional severity for extract rules, or how should this
        # be handled?
        return out.MatchSeverity.from_json(self._raw.get("severity", "INFO"))

    @property
    def mode(self) -> str:
        return self._mode

    @property
    def project_depends_on(self) -> List[Dict[str, str]]:
        if "r2c-internal-project-depends-on" in self._raw:
            depends_on = self._raw["r2c-internal-project-depends-on"]
            if "depends-on-either" in depends_on:
                dependencies: List[Dict[str, str]] = depends_on["depends-on-either"]
                return dependencies
            else:
                return [depends_on]
        else:
            return []

    @property
    def ecosystems(self) -> Set[Ecosystem]:
        if "r2c-internal-project-depends-on" in self._raw:
            depends_on = self._raw["r2c-internal-project-depends-on"]
            if "depends-on-either" in depends_on:
                dependencies: List[Dict[str, str]] = depends_on["depends-on-either"]
                return {
                    Ecosystem.from_json(d["namespace"].lower()) for d in dependencies
                }
            else:
                return {Ecosystem.from_json(depends_on["namespace"].lower())}
        return set()

    @property
    def languages(self) -> List[Language]:
        return self._languages

    @property
    def languages_span(self) -> Span:
        if self._yaml:
            return self._yaml.value["languages"].span
        return EmptySpan

    @property
    def raw(self) -> Dict[str, Any]:
        return self._raw

    @property
    def fix(self) -> Optional[str]:
        return self._raw.get("fix")

    @classmethod
    def from_json(cls, rule_json: Dict[str, Any]) -> "Rule":
        return cls(rule_json, None)

    @classmethod
    def from_yamltree(cls, rule_yaml: YamlTree[YamlMap]) -> "Rule":
        return cls(rule_yaml.unroll_dict(), rule_yaml)

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} id={self.id}>"

    def rename_id(self, new_id: str) -> None:
        self._id = new_id
        self._raw["id"] = new_id

    @property
    def full_hash(self) -> str:
        """
        sha256 hash of the whole rule object instead of just the id.

        We remove metadata cause it can contain user-specific data when connected to Semgrep App.
        """
        rule_dict = self._raw.copy()
        rule_dict.pop("metadata", None)
        return hashlib.sha256(
            json.dumps(rule_dict, sort_keys=True).encode()
        ).hexdigest()

    @property
    def should_run_on_semgrep_core(self) -> bool:
        """
        Used to detect whether the rule had patterns that need to run on the core
        (beyond Python-handled patterns, like `project-depends-on`).
        Remove this code once all rule runnning is done in the core and the answer is always 'yes'
        """
        return any(key in RuleValidation.PATTERN_KEYS for key in self._raw)

    @property
    def product(self) -> out.Product:
        if "r2c-internal-project-depends-on" in self._raw:
            return out.Product(out.SCA())
        elif "product" in self.metadata:
            if self.metadata.get("product") == "secrets":
                return out.Product(out.Secrets())
            else:
                return out.Product(out.SAST())
        else:
            return out.Product(out.SAST())

    @property
    def scan_source(self) -> RuleScanSource:
        src: str = self.metadata.get("semgrep.dev", {}).get("src", "")
        if src == "unchanged":
            return RuleScanSource.unchanged
        elif src == "new-version":
            return RuleScanSource.new_version
        elif src == "new-rule":
            return RuleScanSource.new_rule
        elif src == "previous-scan":
            return RuleScanSource.previous_scan
        else:
            return RuleScanSource.unannotated

    @property
    def from_transient_scan(self) -> bool:
        return self.scan_source == RuleScanSource.previous_scan

    @property
    def formula_string(self) -> str:
        """
        Used to calculate a pattern based ID, works through DFS of all
        PATTERN KEYS, and additionally, if the key is 'join' we include the
        'on' field also.
        """

        # Depth first traversal of formula, where we first sort by pattern keys
        # and where that is not applicable, we do a DFS of two equal keys and
        # sort them by their resulting strings. I.e. sort by pattern key first
        # then any conflicts are resoled by the whole content of the pattern
        def get_subrules(raw: Union[AnyStr, Dict, List]) -> str:
            patterns_to_add = []
            if isinstance(raw, str):
                patterns_to_add.append(raw)
            elif isinstance(raw, dict):
                for key in sorted(raw.keys()):
                    next_raw = raw.get(key)
                    if next_raw is not None:
                        patterns_to_add.append(get_subrules(next_raw))
            elif isinstance(raw, list):
                for p in raw:
                    patterns_to_add.append(get_subrules(p))
            else:
                raise ValueError(
                    f"This rule contains an unexpected pattern key: {self.id}:\n {str(raw)}"
                )
            return " ".join(sorted(patterns_to_add))

        try:
            patterns_to_add = []
            for k in sorted(RuleValidation.PATTERN_KEYS):
                next_raw = self.raw.get(k)
                if next_raw is not None:
                    # print(k)
                    # print(next_raw)
                    patterns_to_add.append(get_subrules(next_raw))
                    if k == "join" and "on" in next_raw:
                        patterns_to_add += get_subrules(next_raw["on"])
            # print(patterns_to_add)
            res = " ".join(sorted(patterns_to_add))
            if not res:
                raise ValueError(f"This rule contains no hashable patterns: {self.id}")
        # In the scenario where we don't have patterns to hash (this would be
        # /weird/), just return the empty string, and we'll hash based on rule id +
        # index + file path
        except ValueError:
            res = ""
        return res


def rule_without_metadata(rule: Rule) -> Rule:
    """Key used to deduplicate rules."""
    new_rule = Rule(rule._raw.copy())
    new_rule._raw.pop("metadata", None)
    return new_rule
