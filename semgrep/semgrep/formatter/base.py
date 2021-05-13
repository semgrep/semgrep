import abc
from typing import Any
from typing import Dict
from typing import FrozenSet
from typing import List

from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


class BaseFormatter(abc.ABC):
    def __init__(
        self,
        rules: FrozenSet[Rule],
        rule_matches: List[RuleMatch],
        semgrep_structured_errors: List[SemgrepError],
        extra: Dict[str, Any],
    ) -> None:
        self.rules = rules
        self.rule_matches = rule_matches
        self.semgrep_structured_errors = semgrep_structured_errors
        self.extra = extra

    @abc.abstractmethod
    def output(self) -> str:
        raise NotImplementedError
