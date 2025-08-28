#
# Copyright (c) 2021-2024 Semgrep Inc.
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
from typing import Any
from typing import Iterable
from typing import Mapping
from typing import Sequence

import semgrep.formatter.base as base
import semgrep.rpc_call
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


class EmacsFormatter(base.BaseFormatter):
    def format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
        ctx: out.FormatContext,
    ) -> str:
        output = base.to_CliOutput(
            rule_matches, semgrep_structured_errors, cli_output_extra
        )
        return semgrep.rpc_call.format(out.OutputFormat(out.Emacs()), ctx, output)
