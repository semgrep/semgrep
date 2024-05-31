import json
from typing import Any
from typing import Dict
from typing import Iterable
from typing import Mapping
from typing import Optional
from typing import Sequence

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep import __VERSION__
from semgrep.error import error_type_string
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


class SarifFormatter(BaseFormatter):
    @staticmethod
    def _taint_source_to_thread_flow_location_sarif(rule_match: RuleMatch) -> Any:
        dataflow_trace = rule_match.dataflow_trace
        if not dataflow_trace:
            return None
        taint_source = dataflow_trace.taint_source
        if not taint_source:
            return None
        if isinstance(taint_source.value, out.CliCall):
            logger.error(
                "Emitting SARIF output for unsupported dataflow trace (source is a call)"
            )
            return None
        elif isinstance(taint_source.value, out.CliLoc):
            taint_source_value = taint_source.value.value.value
            location = taint_source_value[0]
            content = "".join(taint_source_value[1]).strip()
            source_message_text = f"Source: '{content}' @ '{str(location.path.value)}:{str(location.start.line)}'"

            taint_source_location_sarif = {
                "location": {
                    "message": {"text": source_message_text},
                    "physicalLocation": {
                        "artifactLocation": {"uri": str(rule_match.path)},
                        "region": {
                            "startLine": location.start.line,
                            "startColumn": location.start.col,
                            "endLine": location.end.line,
                            "endColumn": location.end.col,
                            "snippet": {"text": content},
                            "message": {"text": source_message_text},
                        },
                    },
                },
                "nestingLevel": 0,
            }
            return taint_source_location_sarif

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
            content = "".join(intermediate_var.content).strip()
            propagation_message_text = f"Propagator : '{content}' @ '{str(location.path.value)}:{str(location.start.line)}'"

            intermediate_vars_location_sarif = {
                "location": {
                    "message": {"text": propagation_message_text},
                    "physicalLocation": {
                        "artifactLocation": {"uri": str(rule_match.path)},
                        "region": {
                            "startLine": location.start.line,
                            "startColumn": location.start.col,
                            "endLine": location.end.line,
                            "endColumn": location.end.col,
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
    def _sink_to_thread_flow_location_sarif(rule_match: RuleMatch) -> Any:
        content = "".join(rule_match.get_lines()).strip()
        sink_message_text = (
            f"Sink: '{content}' @ '{str(rule_match.path)}:{str(rule_match.start.line)}'"
        )

        sink_location_sarif = {
            "location": {
                "message": {"text": sink_message_text},
                "physicalLocation": {
                    "artifactLocation": {"uri": str(rule_match.path)},
                    "region": {
                        "startLine": rule_match.start.line,
                        "startColumn": rule_match.start.col,
                        "endLine": rule_match.end.line,
                        "endColumn": rule_match.end.col,
                        "snippet": {"text": "".join(rule_match.lines).rstrip()},
                        "message": {"text": sink_message_text},
                    },
                },
            },
            "nestingLevel": 1,
        }
        return sink_location_sarif

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
            locations.append(
                SarifFormatter._taint_source_to_thread_flow_location_sarif(rule_match)
            )

        if intermediate_vars:
            intermediate_var_locations = (
                SarifFormatter._intermediate_vars_to_thread_flow_location_sarif(
                    rule_match
                )
            )
            if intermediate_var_locations:
                for intermediate_var_location in intermediate_var_locations:
                    locations.append(intermediate_var_location)

        locations.append(SarifFormatter._sink_to_thread_flow_location_sarif(rule_match))

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
            logger.error(
                "Emitting SARIF output for unsupported dataflow trace (source is a call)"
            )
            return None
        elif isinstance(taint_source.value, out.CliLoc):
            location = taint_source.value.value.value[0]
            code_flow_message = f"Untrusted dataflow from {str(location.path.value)}:{str(location.start.line)} to {str(rule_match.path)}:{str(rule_match.start.line)}"
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
                            "startColumn": rule_match.start.col,
                            "endLine": rule_match.end.line,
                            "endColumn": rule_match.end.col,
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
                                "startColumn": rule_match.start.col,
                                "endLine": rule_match.end.line,
                                "endColumn": rule_match.end.col,
                            },
                            "insertedContent": {"text": "\n".join(fixed_lines)},
                        }
                    ],
                }
            ],
        }
        return fix_sarif

    @staticmethod
    def _rule_to_sarif(rule: Rule, hide_nudge: bool) -> Mapping[str, Any]:
        severity = SarifFormatter._rule_to_sarif_severity(rule)
        tags = SarifFormatter._rule_to_sarif_tags(rule)

        rule_url = rule.metadata.get("source")
        rule_short_description = rule.metadata.get("shortDescription")
        rule_help_text = rule.metadata.get("help") or rule.message or ""
        rule_short_description = rule.metadata.get("shortDescription")
        security_severity = rule.metadata.get("security-severity")

        nudge_base = "💎 Enable cross-file analysis and Pro rules for free at"
        nudge_url = "sg.run/pro"
        nudge_plaintext = f"{nudge_base} {nudge_url}"
        nudge_md = f"#### {nudge_base} <a href='https://{nudge_url}'>{nudge_url}</a>"

        properties = {
            "precision": "very-high",
            "tags": tags,
        }

        if security_severity is not None:
            properties["security-severity"] = security_severity

        rule_json = {
            "id": rule.id,
            "name": rule.id,
            "shortDescription": {"text": f"Semgrep Finding: {rule.id}"},
            "fullDescription": {"text": rule.message},
            "defaultConfiguration": {"level": severity},
            "properties": properties,
        }

        if rule_short_description:
            rule_json["shortDescription"] = {"text": rule_short_description}

        if rule_url is not None:
            rule_json["helpUri"] = rule_url

        semgrep_reference = f"[Semgrep Rule]({rule_url})" if rule_url else None
        references = [semgrep_reference] if semgrep_reference else []
        other_references = rule.metadata.get("references") or []
        # TODO: Handle cases which aren't URLs in custom rules, wont be a problem semgrep-rules.
        references.extend(
            [f"[{r}]({r})" for r in other_references]
            if isinstance(other_references, list)
            else [f"[{other_references}]({other_references})"]
        )
        references_joined = "".join(f" - {ref}\n" for ref in references)
        references_markdown = (
            ""
            if not references_joined
            else f"\n\n<b>References:</b>\n{references_joined}"
        )
        text_suffix = "" if hide_nudge else f"\n{nudge_plaintext}"
        markdown_interstitial = "" if hide_nudge else f"\n\n{nudge_md}"
        rule_json["help"] = {
            "text": f"{rule_help_text}{text_suffix}",
            "markdown": f"{rule_help_text}{markdown_interstitial}{references_markdown}",
        }

        return rule_json

    @staticmethod
    def _rule_to_sarif_severity(rule: Rule) -> str:
        """
        SARIF v2.1.0-compliant severity string.

        See https://github.com/oasis-tcs/sarif-spec/blob/a6473580/Schemata/sarif-schema-2.1.0.json#L1566
        """
        mapping = {
            out.Info(): "note",
            out.Low(): "note",
            out.Warning(): "warning",
            out.Medium(): "warning",
            out.Error(): "error",
            out.High(): "error",
            out.Critical(): "error",
        }
        return mapping[rule.severity.value]

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
        cli_error = error.to_CliError()
        descriptor: str = error_type_string(cli_error.type_)

        error_to_sarif_level = {
            out.Error_(): "error",
            out.Warning_(): "warning",
            out.Info_(): "note",
        }
        level = error_to_sarif_level[error.level.value]

        message = cli_error.message
        if message is None:
            message = cli_error.long_msg
        if message is None:
            message = cli_error.short_msg or ""

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
        sorted_findings = sorted(rule_matches)
        engine_label = (
            cli_output_extra.engine_requested.value.to_json()
            if cli_output_extra.engine_requested
            else "OSS"
        )
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
        is_using_registry = extra.get("is_using_registry", False)
        is_logged_in = extra.get("is_logged_in", False)
        hide_nudge = is_logged_in or is_pro or not is_using_registry

        output_dict = {
            "$schema": "https://docs.oasis-open.org/sarif/sarif/v2.1.0/os/schemas/sarif-schema-2.1.0.json",
            "version": "2.1.0",
            "runs": [
                {
                    "tool": {
                        "driver": {
                            "name": f"Semgrep {engine_label}",
                            "semanticVersion": __VERSION__,
                            "rules": [
                                self._rule_to_sarif(rule, hide_nudge) for rule in rules
                            ],
                        }
                    },
                    "results": [
                        self._rule_match_to_sarif(rule_match, extra["dataflow_traces"])
                        for rule_match in sorted_findings
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
