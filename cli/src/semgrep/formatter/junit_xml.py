from typing import Any
from typing import cast
from typing import Iterable
from typing import Mapping
from typing import Sequence

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.error import SemgrepError
from semgrep.external.junit_xml import TestCase  # type: ignore[attr-defined]
from semgrep.external.junit_xml import TestSuite  # type: ignore[attr-defined]
from semgrep.external.junit_xml import to_xml_report_string  # type: ignore[attr-defined]
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


class JunitXmlFormatter(BaseFormatter):
    @staticmethod
    def _rule_match_to_test_case(rule_match: RuleMatch) -> TestCase:  # type: ignore
        test_case = TestCase(
            rule_match.rule_id,
            file=str(rule_match.path),
            line=rule_match.start.line,
            classname=str(rule_match.path),
        )
        test_case.add_failure_info(
            message=rule_match.message,
            output="".join(rule_match.lines),
            failure_type=rule_match.severity.value,
        )
        return test_case

    def format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
        is_ci_invocation: bool,
    ) -> str:
        test_cases = [
            self._rule_match_to_test_case(rule_match) for rule_match in rule_matches
        ]
        ts = TestSuite("semgrep results", test_cases)
        return cast(str, to_xml_report_string([ts]))
