#
# Copyright (c) 2023-2025 Semgrep Inc.
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
from attrs import frozen

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.parsing_data import ParsingData
from semgrep.types import TargetInfoAccumulator


# This class exists to wrap some of the output returned by `semgrep-core`, on its way up
# through the call stack.
# This class is easily extendable if we want to add more information to the CLI output
# in the future.
@frozen
class OutputExtra:
    core: out.CoreOutput
    all_targets: TargetInfoAccumulator
    parsing_data: ParsingData
