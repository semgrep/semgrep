from attrs import frozen

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.parsing_data import ParsingData
from semgrep.types import TargetAccumulator


# This class exists to wrap some of the output returned by `semgrep-core`, on its way up
# through the call stack.
# This class is easily extendable if we want to add more information to the CLI output
# in the future.
@frozen
class OutputExtra:
    core: out.CoreOutput
    all_targets: TargetAccumulator
    parsing_data: ParsingData
