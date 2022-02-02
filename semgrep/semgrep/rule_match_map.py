from typing import Dict
from typing import List

from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch

RuleMatchMap = Dict[Rule, List[RuleMatch]]
