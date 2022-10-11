"""
Main function to exclude from list of rules rules with certain id's
"""
from typing import List
from typing import Sequence

from semgrep.rule import Rule


def filter_exclude_rule(rules: List[Rule], exclude_rules: Sequence[str]) -> List[Rule]:
    return list(filter(lambda r: r.id not in exclude_rules, rules))
