import abc
from typing import Any
from typing import Collection
from typing import FrozenSet
from typing import Iterable
from typing import Mapping
from typing import Sequence

import semgrep.semgrep_interfaces.semgrep_output_v0 as out
from semgrep.constants import RuleSeverity
from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


class BaseFormatter(abc.ABC):
    def output(
        self,
        rules: FrozenSet[Rule],
        rule_matches: Sequence[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
        shown_severities: Collection[RuleSeverity],
    ) -> str:
        filtered_rules = (r for r in rules if r.severity in shown_severities)
        filtered_matches = (m for m in rule_matches if m.severity in shown_severities)
        return self.format(
            filtered_rules,
            filtered_matches,
            semgrep_structured_errors,
            cli_output_extra,
            extra,
        )

    @abc.abstractmethod
    def format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
    ) -> str:
        raise NotImplementedError

    def keep_ignores(self) -> bool:
        """
        Return True if ignored findings should be passed to this formatter; False otherwise.

        Ignored findings can still be distinguished using their _is_ignore property.
        """
        return False
