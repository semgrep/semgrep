from pathlib import Path
from typing import List
from typing import Optional
from typing import Set

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
    all_targets: Set[Path]
    profiling_data: ProfilingData
    parsing_data: ParsingData
    explanations: Optional[List[core.MatchingExplanation]]
