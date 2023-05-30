#TODO: PARTIALLY PORTED TO OCAML

from semgrep.error import InvalidRuleSchemaError
from semgrep.rule_lang import EmptySpan
from semgrep.rule_lang import RuleValidation
from semgrep.rule_lang import Span
from semgrep.semgrep_interfaces.semgrep_output_v0 import Ecosystem
from semgrep.semgrep_types import LANGUAGE
from semgrep.semgrep_types import Language

class Rule:
    def __init__() -> None:
        lang_span = (
            yaml.value["languages"].span if yaml and "languages" in yaml.value else None
        )
        rule_languages: Set[Language] = {
            LANGUAGE.resolve(l, lang_span) for l in self._raw.get("languages", [])
        }

        # add typescript to languages if the rule supports javascript.
        # TODO: Move this hack to lang.json
        if any(
            language == LANGUAGE.resolve("javascript") for language in rule_languages
        ):
            rule_languages.add(LANGUAGE.resolve("typescript"))
            self._raw["languages"] = sorted(str(l) for l in rule_languages)

        self._languages = sorted(rule_languages)


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

    @property
    def is_blocking(self) -> bool:
        """
        Returns if this rule indicates matches should block CI
        """
        return "block" in self.metadata.get("dev.semgrep.actions", ["block"])

    @property
    def severity(self) -> RuleSeverity:
        # TODO: add additional severity for extract rules, or how should this
        # be handled?
        return RuleSeverity(self._raw.get("severity", RuleSeverity.INFO))

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
    def languages_span(self) -> Span:
        if self._yaml:
            return self._yaml.value["languages"].span
        return EmptySpan

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
                    patterns_to_add.append(get_subrules(next_raw))
                    if k == "join" and "on" in next_raw:
                        patterns_to_add += get_subrules(next_raw["on"])
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
