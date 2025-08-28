#
# Copyright (c) 2022-2024 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
"""
Main function to exclude from list of rules rules with certain id's
"""
from typing import List
from typing import Sequence

from semgrep.rule import Rule


def filter_exclude_rule(rules: List[Rule], exclude_rules: Sequence[str]) -> List[Rule]:
    return list(filter(lambda r: r.id not in exclude_rules, rules))
