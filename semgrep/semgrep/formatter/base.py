import abc
from typing import Any
from typing import FrozenSet
from typing import Mapping
from typing import Sequence

from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


class BaseFormatter(abc.ABC):
    @abc.abstractmethod
    def output(
        self,
        rules: FrozenSet[Rule],
        rule_matches: Sequence[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        extra: Mapping[str, Any],
    ) -> str:
        raise NotImplementedError
