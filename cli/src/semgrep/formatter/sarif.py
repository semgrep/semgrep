import contextlib
import json
import tempfile
from typing import Any
from typing import Iterable
from typing import Mapping
from typing import Optional
from typing import Sequence

import semgrep.formatter.base as base
import semgrep.rpc_call
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


class SarifFormatter(base.BaseFormatter):
    def keep_ignores(self) -> bool:
        return True

    # TODO: remove many params, use just output and ctx
    # so we can get sarif RPC be the same than other formatter RPC
    # and remove this intermediate function and inline code in format() below
    def _osemgrep_format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
        ctx: out.FormatContext,
    ) -> Optional[out.SarifFormatReturn]:
        exit_stack = contextlib.ExitStack()
        with exit_stack:
            rule_file = exit_stack.enter_context(
                tempfile.NamedTemporaryFile("w+", suffix=".json")
            )
            rule_file_contents = json.dumps(
                {"rules": [rule._raw for rule in rules]}, indent=2, sort_keys=True
            )
            rule_file.write(rule_file_contents)
            rule_file.flush()
            rules_path = out.Fpath(rule_file.name)

            """
            Exclude Semgrep notice for users who
            1. log in
            2. use pro engine
            3. are not using registry
            """
            is_pro = (
                cli_output_extra.engine_requested
                and cli_output_extra.engine_requested == out.EngineKind(out.PRO_())
            )
            hide_nudge = ctx.is_logged_in or is_pro or not ctx.is_using_registry

            engine_label = "PRO" if is_pro else "OSS"

            show_dataflow_traces = extra.get("dataflow_traces", False)

            # Sort according to RuleMatch.get_ordering_key
            sorted_findings = sorted(rule_matches)
            cli_matches = [
                base.rule_match_to_CliMatch(rule_match)
                for rule_match in sorted_findings
            ]
            cli_errors = [e.to_CliError() for e in semgrep_structured_errors]

            rpc_params = out.SarifFormatParams(
                hide_nudge=hide_nudge,
                engine_label=engine_label,
                rules=rules_path,
                cli_matches=cli_matches,
                cli_errors=cli_errors,
                show_dataflow_traces=show_dataflow_traces,
            )
            formatted_output = semgrep.rpc_call.sarif_format(ctx, rpc_params)
            if formatted_output:
                return formatted_output.value
        return None

    def format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
        ctx: out.FormatContext,
    ) -> str:
        # TODO: use regular OutputFormat RPC but SARIF needs a few
        #   more things such as the rules so we use a different RPC for now
        # output = base.to_CliOutput(
        #    rule_matches, semgrep_structured_errors, cli_output_extra
        # )
        # return semgrep.rpc_call.format(out.OutputFormat(out.Sarif()), ctx, output)
        rule_list = list(rules)
        rule_match_list = list(rule_matches)
        error_list = list(semgrep_structured_errors)
        rpc_result = self._osemgrep_format(
            rule_list, rule_match_list, error_list, cli_output_extra, extra, ctx
        )
        if rpc_result is not None:
            o_output = rpc_result.output
            return o_output
        else:
            return "<ERROR: no SARIF output>"
