"""
Main function to exclude from list of rules rules with certain id's
"""
from __future__ import annotations

from typing import Sequence

from semgrep.rule import Rule


def filter_exclude_rule(rules: list[Rule], exclude_rules: Sequence[str]) -> list[Rule]:
    return list(filter(lambda r: r.id not in exclude_rules, rules))
