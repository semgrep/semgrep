import collections
from pathlib import Path
from typing import Dict
from typing import FrozenSet
from typing import Iterator
from typing import List
from typing import Tuple

from semgrep.rule import Rule
from semgrep.semgrep_types import Language
from semgrep.target_manager import TargetManager


class CoreOrchestrator:
    """
    Optimizes which rules, language, targets to pass to run together on a single
    invokation of semgrep-core
    """

    def _group_rules_by_language(self, rules: List[Rule]) -> Dict[Language, List[Rule]]:
        by_language: Dict[Language, List[Rule]] = collections.defaultdict(list)
        for rule in rules:
            for language in rule.languages:
                by_language[language].append(rule)

        return by_language

    def orchestrate(
        self, rules: List[Rule], target_manager: TargetManager
    ) -> Iterator[Tuple[Language, List[Rule], FrozenSet[Path]]]:
        yield from self._group_rules_targets(rules, target_manager)

    def _group_rules_targets(
        self, rules: List[Rule], target_manager: TargetManager
    ) -> Iterator[Tuple[Language, List[Rule], FrozenSet[Path]]]:
        rules_by_language = self._group_rules_by_language(rules)

        lang_to_targets_to_rules = {}

        for language, rules_of_language in rules_by_language.items():
            targets_to_rules = collections.defaultdict(list)

            for rule in rules_of_language:
                targets = target_manager.get_files(
                    language, rule.includes, rule.excludes
                )
                targets_to_rules[targets].append(rule)

            lang_to_targets_to_rules[language] = targets_to_rules

        for language, targets_to_rules in lang_to_targets_to_rules.items():
            for targets, rules_ in targets_to_rules.items():
                # for rule in rules_:
                # 	yield language, [rule], targets
                yield language, rules_, targets
