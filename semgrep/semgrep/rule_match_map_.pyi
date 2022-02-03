from typing import Any, Dict, List, Sequence, Set, Tuple, Optional, Collection
from typing import Mapping
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch

RuleMatchMap = Mapping[Rule, Sequence[RuleMatch]]
