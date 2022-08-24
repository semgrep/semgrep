from typing import List, Sequence
from semgrep.rule import Rule


def filter_exclude_rule(rules: List[Rule], exclude_rules: Sequence[str]) -> List[Rule]:
    return list(filter(lambda r: r.id not in exclude_rules, rules))
