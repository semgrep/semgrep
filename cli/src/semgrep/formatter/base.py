import abc
from typing import Any
from typing import Collection
from typing import FrozenSet
from typing import Iterable
from typing import Mapping
from typing import Sequence

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


def rule_match_to_CliMatch(rule_match: RuleMatch) -> out.CliMatch:
    extra = out.CliMatchExtra(
        message=rule_match.message,
        metadata=out.RawJson(rule_match.metadata),
        severity=rule_match.severity,
        fingerprint=rule_match.match_based_id,
        # 'lines' already contains '\n' at the end of each line
        lines="".join(rule_match.lines).rstrip(),
        metavars=rule_match.match.extra.metavars,
        dataflow_trace=rule_match.dataflow_trace,
        engine_kind=rule_match.match.extra.engine_kind,
        validation_state=rule_match.match.extra.validation_state,
    )

    if rule_match.extra.get("sca_info"):
        extra.sca_info = rule_match.extra.get("sca_info")
    if rule_match.extra.get("fixed_lines"):
        extra.fixed_lines = rule_match.extra.get("fixed_lines")
    if rule_match.fix is not None:
        extra.fix = rule_match.fix
    if rule_match.is_ignored is not None:
        extra.is_ignored = rule_match.is_ignored
    if rule_match.extra.get("extra_extra"):
        extra.extra_extra = out.RawJson(rule_match.extra.get("extra_extra"))

    return out.CliMatch(
        check_id=out.RuleId(rule_match.rule_id),
        path=out.Fpath(str(rule_match.path)),
        start=rule_match.start,
        end=rule_match.end,
        extra=extra,
    )


class BaseFormatter(abc.ABC):
    def output(
        self,
        rules: FrozenSet[Rule],
        rule_matches: Sequence[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
        shown_severities: Collection[out.MatchSeverity],
        is_ci_invocation: bool,
    ) -> str:
        filtered_rules = (r for r in rules if r.severity in shown_severities)
        filtered_matches = (m for m in rule_matches if m.severity in shown_severities)
        return self.format(
            filtered_rules,
            filtered_matches,
            semgrep_structured_errors,
            cli_output_extra,
            extra,
            is_ci_invocation,
        )

    @abc.abstractmethod
    def format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
        is_ci_invocation: bool,
    ) -> str:
        raise NotImplementedError

    def keep_ignores(self) -> bool:
        """
        Return True if ignored findings should be passed to this formatter; False otherwise.

        Ignored findings can still be distinguished using their _is_ignore property.
        """
        return False
