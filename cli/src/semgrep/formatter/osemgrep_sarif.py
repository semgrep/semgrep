import contextlib
import difflib
import json
import tempfile
import timeit
from typing import Any
from typing import Iterable
from typing import Mapping
from typing import Optional
from typing import Sequence

import semgrep.ocaml as ocaml
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.formatter.base import rule_match_to_CliMatch
from semgrep.formatter.sarif import SarifFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_interfaces.semgrep_metrics import OsemgrepFormatOutput
from semgrep.state import get_state
from semgrep.verbose_logging import getLogger


logger = getLogger(__name__)


# Rules may be ordered differently in python and ocaml, but it
# shouldn't matter to the user, so we will sort the rules by rule id
# just for validating purposes.
def normalize_sarif_findings(findings: Mapping, mode: str) -> Mapping:
    try:
        for run in findings["runs"]:
            sorted_rules = sorted(
                run["tool"]["driver"]["rules"], key=lambda rule: rule["id"]
            )
            run["tool"]["driver"]["rules"] = sorted_rules
    except KeyError as e:
        logger.verbose(f"Invalid SARIF format ({mode}): key not found {e}")
    return findings


class OsemgrepSarifFormatter(BaseFormatter):
    def _osemgrep_format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
        _is_ci_invocation: bool,
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
            is_logged_in = extra.get("is_logged_in", False)
            is_pro = (
                cli_output_extra.engine_requested
                and cli_output_extra.engine_requested == out.EngineKind(out.PRO_())
            )
            is_using_registry = extra.get("is_using_registry", False)
            hide_nudge = is_logged_in or is_pro or not is_using_registry

            engine_label = "PRO" if is_pro else "OSS"

            show_dataflow_traces = extra["dataflow_traces"]

            # Sort according to RuleMatch.get_ordering_key
            sorted_findings = sorted(rule_matches)
            cli_matches = [
                rule_match_to_CliMatch(rule_match) for rule_match in sorted_findings
            ]
            cli_errors = [e.to_CliError() for e in semgrep_structured_errors]

            rpc_params = out.SarifFormatParams(
                hide_nudge,
                engine_label,
                rules_path,
                cli_matches,
                cli_errors,
                show_dataflow_traces,
            )
            formatted_output = ocaml.sarif_format(rpc_params)
            if formatted_output:
                return formatted_output.value
        return None

    def keep_ignores(self) -> bool:
        # SARIF output includes ignored findings, but labels them as suppressed.
        # https://docs.oasis-open.org/sarif/sarif/v2.1.0/csprd01/sarif-v2.1.0-csprd01.html#_Toc10541099
        return True

    def format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
        is_ci_invocation: bool,
    ) -> str:
        rule_list = list(rules)
        rule_match_list = list(rule_matches)
        error_list = list(semgrep_structured_errors)
        rpc_start = timeit.default_timer()
        rpc_result = self._osemgrep_format(
            rule_list,
            rule_match_list,
            error_list,
            cli_output_extra,
            extra,
            is_ci_invocation,
        )
        rpc_elapse = timeit.default_timer() - rpc_start

        pysemgrep_formatter = SarifFormatter()
        py_start = timeit.default_timer()
        py_output = pysemgrep_formatter.format(
            rule_list,
            rule_match_list,
            error_list,
            cli_output_extra,
            extra,
            is_ci_invocation,
        )
        py_elapse = timeit.default_timer() - py_start

        succeeded = False
        is_match = None
        validate_elapse = None
        o_elapse = None
        if rpc_result is not None:
            succeeded = True
            o_elapse = rpc_result.format_time_seconds

            # Validate results and time it to make sure it's not expensive.
            validate_start = timeit.default_timer()
            o_output = rpc_result.output
            o_json = normalize_sarif_findings(json.loads(o_output), "osemgrep")
            py_json = normalize_sarif_findings(json.loads(py_output), "pysemgrep")
            is_match = o_json == py_json
            if not is_match:
                # Log the difference when there's a mismatch.
                #
                # Be more verbose with --debug. Helpful for local testing.
                #
                # Otherwise cap the log to 10 lines with --verbose,
                # which would be helpful for debugging when things go wrong in CI.
                o_lines = json.dumps(o_json, sort_keys=True, indent=4).split("\n")
                py_lines = json.dumps(py_json, sort_keys=True, indent=4).split("\n")
                if get_state().terminal.is_debug:
                    diffs = difflib.unified_diff(o_lines, py_lines, n=10)
                    logger.debug("diff osemgrep vs pysemgrep:\n" + "\n".join(diffs))
                else:
                    diffs_list = list(difflib.unified_diff(o_lines, py_lines, n=1))
                    max_n_lines = 10
                    lines = "\n".join(diffs_list[:max_n_lines])
                    if len(diffs_list) > max_n_lines:
                        logger.verbose(
                            f"partial diff osemgrep vs pysemgrep (use --debug to see full diff):\n{lines}"
                        )
                    else:
                        logger.verbose(f"diff osemgrep vs pysemgrep:\n{lines}")
            validate_elapse = timeit.default_timer() - validate_start

        # Update metrics so we can keep track of how well the migration is going.
        format_metrics = OsemgrepFormatOutput()
        format_metrics.format = "SARIF"
        format_metrics.succeeded = succeeded
        format_metrics.is_match = is_match
        format_metrics.osemgrep_rpc_response_time_seconds = rpc_elapse
        format_metrics.osemgrep_format_time_seconds = o_elapse
        format_metrics.pysemgrep_format_time_seconds = py_elapse
        format_metrics.validation_time_seconds = validate_elapse
        get_state().metrics.add_osemgrep_format_output_metrics(format_metrics)

        if succeeded and is_match:
            return o_output
        logger.verbose(
            "Osemgrep vs Pysemgrep SARIF output mismatch. Falling back to python output."
        )
        return py_output
