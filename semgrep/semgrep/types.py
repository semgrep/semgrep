from typing import Any
from typing import Mapping
from typing import Sequence

from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch

JsonObject = Mapping[str, Any]

RuleMatchMap = Mapping[Rule, Sequence[RuleMatch]]
