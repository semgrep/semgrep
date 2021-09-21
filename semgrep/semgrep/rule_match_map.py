from typing import Mapping
from typing import Sequence

from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch

RuleMatchMap = Mapping[Rule, Sequence[RuleMatch]]
