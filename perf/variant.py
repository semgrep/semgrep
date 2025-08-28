#
# Copyright (c) 2021-2025 Semgrep Inc.
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
class SemgrepVariant:
    def __init__(self, name: str, semgrep_core_extra: str, semgrep_extra: str = ""):
        # name for the input corpus (rules and targets)
        self.name = name

        # space-separated extra arguments to pass to semgrep-core
        # command via SEMGREP_CORE_EXTRA environment variable
        self.semgrep_core_extra = semgrep_core_extra

        # space-separated extra arguments to pass to the default semgrep
        # command
        self.semgrep_extra = semgrep_extra


# Feel free to create new variants. The idea is to use the default set
# of options as the baseline and we see what happens when we enable or
# disable this or that optimization.
#
from constants import STD

SEMGREP_VARIANTS = [
    # default settings
    SemgrepVariant(STD, ""),
    SemgrepVariant("no-filter-irrelevant-rules", "-no_filter_irrelevant_rules"),
]

# For when you just want to test a single change
STD_VARIANTS = [SemgrepVariant(STD, "")]
