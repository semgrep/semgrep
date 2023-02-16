from __future__ import annotations

from pathlib import Path

from attrs import frozen

import semgrep.output_from_core as core
from semgrep.parsing_data import ParsingData
from semgrep.profiling import ProfilingData


# This class exists to wrap some of the output returned by `semgrep-core`, on its way up
# through the call stack.
# This class is easily extendable if we want to add more information to the CLI output
# in the future.
@frozen
class OutputExtra:
    all_targets: set[Path]
    profiling_data: ProfilingData
    parsing_data: ParsingData
    explanations: list[core.MatchingExplanation] | None
    rules_by_engine: list[core.RuleIdAndEngineKind]
