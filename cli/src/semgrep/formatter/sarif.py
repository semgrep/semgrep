import json
from typing import Any
from typing import Dict
from typing import List
from typing import Iterable
from typing import Mapping
from typing import Optional
from typing import Sequence
from pathlib import Path

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep import __VERSION__
from semgrep.constants import RuleSeverity
from semgrep.error import Level
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.verbose_logging import getLogger
from semgrep.util import get_lines

logger = getLogger(__name__)


class SarifFormatter(BaseFormatter):

    @staticmethod
    def _generate_taint_snippet(location: Any) -> Any:
        content = "".join(get_lines(Path(location.path), location.start.line, location.end.line + 2)).rstrip('\n')
        snippet_before = "".join(
            get_lines(
                Path(location.path),
                location.start.line - 2,
                location.end.line - 1)
        ).lstrip('\n')
        start_column = len(snippet_before) + location.start.col
        end_column = len(snippet_before) + location.end.col
        return "".join([snippet_before, content]), start_column, end_column

    @staticmethod
    def _taint_obj_intermediate_vars_to_thread_flow_location_sarif(intermediate_var: Any, rule_match: RuleMatch) -> Any:
        location = intermediate_var.location
        content, start_column, end_column = SarifFormatter._generate_taint_snippet(location)
        propagation_message_text = f"Propagator : '{content}' @ '{str(location.path)}:{str(location.start.line)}'"

        intermediate_vars_location_sarif = {
            "location": {
                "message": {"text": propagation_message_text},
                "physicalLocation": {
                    "artifactLocation": {"uri": str(rule_match.path)},
                    "region": {
                        "startLine": location.start.line,
                        "startColumn": start_column,
                        "endLine": location.end.line,
                        "endColumn": end_column,
                        "snippet": {"text": content},
                        "message": {"text": propagation_message_text},
                    },
                },
            }
        }
        return intermediate_vars_location_sarif

    @staticmethod
    def _rec_taint_obj_to_thread_flow_location_sarif(var_type: str, taint_obj: Any, rule_match: RuleMatch) -> List[Any]:
        taint_trace = []

        if isinstance(taint_obj, out.CliMatchCallTrace):
            taint_trace += SarifFormatter._rec_taint_obj_to_thread_flow_location_sarif(var_type, taint_obj.value,
                                                                                       rule_match)

        if isinstance(taint_obj, out.CliCall):
            taint_trace += SarifFormatter._rec_taint_obj_to_thread_flow_location_sarif(var_type, taint_obj.value[2],
                                                                                       rule_match)

            for intermediate_var in taint_obj.value[1]:
                taint_trace.append(
                    SarifFormatter._taint_obj_intermediate_vars_to_thread_flow_location_sarif(intermediate_var,
                                                                                              rule_match))

        # the taint object can be an instance of CliLoc, CliClass or CliMatchCallTrace
        if isinstance(taint_obj, out.CliLoc):
            location = taint_obj.value[0]
        elif isinstance(taint_obj, out.CliCall):
            var_type = "Propagator"
            location = taint_obj.value[0][0]
        else:
            return taint_trace

        content, start_column, end_column = SarifFormatter._generate_taint_snippet(location)
        source_message_text = f"{var_type} : '{content}' @ '{str(location.path)}:{str(location.start.line)}'"
        location_sarif = {
            "location": {
                "message": {"text": source_message_text},
                "physicalLocation": {
                    "artifactLocation": {"uri": str(rule_match.path)},
                    "region": {
                        "startLine": location.start.line,
                        "startColumn": start_column,
                        "endLine": location.end.line,
                        "endColumn": end_column,
                        "snippet": {"text": content},
                        "message": {"text": source_message_text},
                    },
                },
            }
        }
        return taint_trace + [location_sarif]

    @staticmethod
    def _taint_source_to_thread_flow_location_sarif(rule_match: RuleMatch) -> Any:
        dataflow_trace = rule_match.dataflow_trace
        if not dataflow_trace:
            return None
        taint_source = dataflow_trace.taint_source
        if not taint_source:
            return None
        # calculate source flow - recursive
        return SarifFormatter._rec_taint_obj_to_thread_flow_location_sarif("Source", taint_source, rule_match)

    @staticmethod
    def _intermediate_vars_to_thread_flow_location_sarif(rule_match: RuleMatch) -> Any:
        dataflow_trace = rule_match.dataflow_trace
        if not dataflow_trace:
            return None
        intermediate_vars = dataflow_trace.intermediate_vars
        if not intermediate_vars:
            return None
        intermediate_var_locations = []
        for intermediate_var in intermediate_vars:
            location = intermediate_var.location
            content, start_column, end_column = SarifFormatter._generate_taint_snippet(location)
            propagation_message_text = f"Propagator : '{content}' @ '{str(location.path.value)}:{str(location.start.line)}'"

            intermediate_vars_location_sarif = {
                "location": {
                    "message": {"text": propagation_message_text},
                    "physicalLocation": {
                        "artifactLocation": {"uri": str(rule_match.path)},
                        "region": {
                            "startLine": location.start.line,
                            "startColumn": start_column,
                            "endLine": location.end.line,
                            "endColumn": end_column,
                            "snippet": {"text": content},
                            "message": {"text": propagation_message_text},
                        },
                    },
                },
                "nestingLevel": 0,
            }
            intermediate_var_locations.append(intermediate_vars_location_sarif)
        return intermediate_var_locations

    @staticmethod
    def _dataflow_trace_to_thread_flows_sarif(rule_match: RuleMatch) -> Any:
        thread_flows = []
        locations = []

        dataflow_trace = rule_match.dataflow_trace
        if not dataflow_trace:
            return None
        taint_source = dataflow_trace.taint_source
        # TODO: deal with taint sink
        intermediate_vars = dataflow_trace.intermediate_vars

        if taint_source:
            # calculate intermediate vars/calls for the source
            locations += SarifFormatter._taint_source_to_thread_flow_location_sarif(rule_match)

        if intermediate_vars:
            intermediate_var_locations = (
                SarifFormatter._intermediate_vars_to_thread_flow_location_sarif(
                    rule_match
                )
            )
            if intermediate_var_locations:
                for intermediate_var_location in intermediate_var_locations:
                    locations.append(intermediate_var_location)

        sink_thread_trace = SarifFormatter._rec_taint_obj_to_thread_flow_location_sarif("Sink",
                                                                                        dataflow_trace.taint_sink,
                                                                                        rule_match)[::-1]
        locations += sink_thread_trace

        thread_flows.append({"locations": locations})
        return thread_flows

    @staticmethod
    def _dataflow_trace_to_codeflow_sarif(
            rule_match: RuleMatch,
    ) -> Optional[Mapping[str, Any]]:
        dataflow_trace = rule_match.dataflow_trace
        if not dataflow_trace:
            return None
        taint_source = dataflow_trace.taint_source
        if not taint_source:
            return None

        # TODO: handle rule_match.taint_sink
        if isinstance(taint_source.value, out.CliCall):
            location = taint_source.value.value[0][0]
            code_flow_message = f"Untrusted dataflow from {str(location.path)}:{str(location.start.line)} to {str(rule_match.path)}:{str(rule_match.start.line)}"
        elif isinstance(taint_source.value, out.CliLoc):
            location = taint_source.value.value[0]
            code_flow_message = f"Untrusted dataflow from {str(location.path)}:{str(location.start.line)} to {str(rule_match.path)}:{str(rule_match.start.line)}"

        code_flow_sarif = {
            "message": {"text": code_flow_message},
        }
        thread_flows = SarifFormatter._dataflow_trace_to_thread_flows_sarif(
            rule_match
        )
        if thread_flows:
            code_flow_sarif["threadFlows"] = thread_flows

        return code_flow_sarif

    @staticmethod
    def _rule_match_to_sarif(
        rule_match: RuleMatch, dataflow_traces: bool
    ) -> Mapping[str, Any]:
        rule_match_sarif: Dict[str, Any] = {
            "ruleId": rule_match.rule_id,
            "message": {"text": rule_match.message},
            "locations": [
                {
                    "physicalLocation": {
                        "artifactLocation": {
                            "uri": str(rule_match.path),
                            "uriBaseId": "%SRCROOT%",
                        },
                        "region": {
                            "snippet": {"text": "".join(rule_match.lines).rstrip()},
                            "startLine": rule_match.start.line,
                            "startColumn": rule_match.start.col - 1,
                            "endLine": rule_match.end.line,
                            "endColumn": rule_match.end.col - 1,
                        },
                    }
                }
            ],
            "fingerprints": {"matchBasedId/v1": rule_match.match_based_id},
            "properties": {},
        }

        if dataflow_traces and rule_match.dataflow_trace:
            code_flows = SarifFormatter._dataflow_trace_to_codeflow_sarif(rule_match)
            if code_flows:
                rule_match_sarif["codeFlows"] = [code_flows]

        if rule_match.is_ignored:
            rule_match_sarif["suppressions"] = [{"kind": "inSource"}]

        fix = SarifFormatter._rule_match_to_sarif_fix(rule_match)

        if fix is not None:
            rule_match_sarif["fixes"] = [fix]

        if rule_match.exposure_type:
            rule_match_sarif["properties"]["exposure"] = rule_match.exposure_type

        return rule_match_sarif

    @staticmethod
    def _rule_match_to_sarif_fix(rule_match: RuleMatch) -> Optional[Mapping[str, Any]]:
        # if rule_match.extra.get("dependency_matches"):
        fixed_lines = rule_match.extra.get("fixed_lines")

        description = "Semgrep rule suggested fix"
        if not fixed_lines:
            return None
        description_text = f"{rule_match.message}\n Autofix: {description}"
        fix_sarif = {
            "description": {"text": description_text},
            "artifactChanges": [
                {
                    "artifactLocation": {"uri": str(rule_match.path)},
                    "replacements": [
                        {
                            "deletedRegion": {
                                "startLine": rule_match.start.line,
                                "startColumn": rule_match.start.col - 1,
                                "endLine": rule_match.end.line,
                                "endColumn": rule_match.end.col - 1,
                            },
                            "insertedContent": {"text": "\n".join(fixed_lines)},
                        }
                    ],
                }
            ],
        }
        return fix_sarif

    @staticmethod
    def _rule_to_sarif(rule: Rule) -> Mapping[str, Any]:
        severity = SarifFormatter._rule_to_sarif_severity(rule)
        tags = SarifFormatter._rule_to_sarif_tags(rule)
        security_severity = rule.metadata.get("security-severity")

        if security_severity is not None:
            rule_json = {
                "id": rule.id,
                "name": rule.id,
                "shortDescription": {"text": f"Semgrep Finding: {rule.id}"},
                "fullDescription": {"text": rule.message},
                "defaultConfiguration": {"level": severity},
                "properties": {
                    "precision": "very-high",
                    "tags": tags,
                    "security-severity": security_severity,
                },
            }
        else:
            rule_json = {
                "id": rule.id,
                "name": rule.id,
                "shortDescription": {"text": f"Semgrep Finding: {rule.id}"},
                "fullDescription": {"text": rule.message},
                "defaultConfiguration": {"level": severity},
                "properties": {"precision": "very-high", "tags": tags},
            }

        rule_url = rule.metadata.get("source")
        references = []

        if rule_url is not None:
            rule_json["helpUri"] = rule_url
            references.append(f"[Semgrep Rule]({rule_url})")

        if rule.metadata.get("references"):
            ref = rule.metadata["references"]
            # TODO: Handle cases which aren't URLs in custom rules, wont be a problem semgrep-rules.
            references.extend(
                [f"[{r}]({r})" for r in ref]
                if isinstance(ref, list)
                else [f"[{ref}]({ref})"]
            )
        if references:
            r = "".join(
                [f" - {references_markdown}\n" for references_markdown in references]
            )
            rule_json["help"] = {
                "text": rule.message,
                "markdown": f"{rule.message}\n\n<b>References:</b>\n{r}",
            }
        rule_short_description = rule.metadata.get("shortDescription")
        if rule_short_description:
            rule_json["shortDescription"] = {"text": rule_short_description}

        rule_help_text = rule.metadata.get("help")
        if rule_help_text:
            rule_json["help"] = {"text": rule_help_text}

        return rule_json

    @staticmethod
    def _rule_to_sarif_severity(rule: Rule) -> str:
        """
        SARIF v2.1.0-compliant severity string.

        See https://github.com/oasis-tcs/sarif-spec/blob/a6473580/Schemata/sarif-schema-2.1.0.json#L1566
        """
        mapping = {
            RuleSeverity.INFO: "note",
            RuleSeverity.WARNING: "warning",
            RuleSeverity.ERROR: "error",
        }
        return mapping[rule.severity]

    @staticmethod
    def _rule_to_sarif_tags(rule: Rule) -> Sequence[str]:
        """
        Tags to display on SARIF-compliant UIs, such as GitHub security scans.
        """
        result = []
        if "cwe" in rule.metadata:
            cwe = rule.metadata["cwe"]
            result.extend(cwe if isinstance(cwe, list) else [cwe])
            result.append("security")
        if "owasp" in rule.metadata:
            owasp = rule.metadata["owasp"]
            result.extend(
                [f"OWASP-{o}" for o in owasp]
                if isinstance(owasp, list)
                else [f"OWASP-{owasp}"]
            )
        if rule.metadata.get("confidence"):
            confidence = rule.metadata["confidence"]
            result.append(f"{confidence} CONFIDENCE")
        if (
            "semgrep.policy" in rule.metadata
            and "slug" in rule.metadata["semgrep.policy"]
        ):
            # https://github.com/returntocorp/semgrep-app/blob/8d2e6187b7daa2b20c49839a4fcb67e560202aa8/frontend/src/pages/ruleBoard/constants/constants.tsx#L74
            # this should be "rule-board-audit", "rule-board-block", or "rule-board-pr-comments"
            slug = rule.metadata["semgrep.policy"]["slug"]
            result.append(slug)

        for tags in rule.metadata.get("tags", []):
            result.append(tags)

        return sorted(set(result))

    @staticmethod
    def _semgrep_error_to_sarif_notification(error: SemgrepError) -> Mapping[str, Any]:
        error_dict = error.to_dict()
        descriptor = error_dict["type"]

        error_to_sarif_level = {
            Level.ERROR.name.lower(): "error",
            Level.WARN.name.lower(): "warning",
        }
        level = error_to_sarif_level[error_dict["level"]]

        message = error_dict.get("message")
        if message is None:
            message = error_dict.get("long_msg")
        if message is None:
            message = error_dict.get("short_msg", "")

        return {
            "descriptor": {"id": descriptor},
            "message": {"text": message},
            "level": level,
        }

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
        """
        Format matches in SARIF v2.1.0 formatted JSON.

        - Written based on:
            https://help.github.com/en/github/finding-security-vulnerabilities-and-errors-in-your-code/about-sarif-support-for-code-scanning
        - Which links to this schema:
            https://github.com/oasis-tcs/sarif-spec/blob/master/Schemata/sarif-schema-2.1.0.json
        - Full specification is at:
            https://docs.oasis-open.org/sarif/sarif/v2.1.0/cs01/sarif-v2.1.0-cs01.html
        """

        output_dict = {
            "$schema": "https://docs.oasis-open.org/sarif/sarif/v2.1.0/os/schemas/sarif-schema-2.1.0.json",
            "version": "2.1.0",
            "runs": [
                {
                    "tool": {
                        "driver": {
                            "name": "semgrep",
                            "semanticVersion": __VERSION__,
                            "rules": [self._rule_to_sarif(rule) for rule in rules],
                        }
                    },
                    "results": [
                        self._rule_match_to_sarif(rule_match, extra["dataflow_traces"])
                        for rule_match in rule_matches
                    ],
                    "invocations": [
                        {
                            "executionSuccessful": True,
                            "toolExecutionNotifications": [
                                self._semgrep_error_to_sarif_notification(error)
                                for error in semgrep_structured_errors
                            ],
                        }
                    ],
                },
            ],
        }

        # Sort keys for predictable output. This helps with snapshot tests, etc.
        return json.dumps(output_dict, sort_keys=True, indent=2)
