from typing import cast

from semgrep.external.junit_xml import TestCase  # type: ignore[attr-defined]
from semgrep.external.junit_xml import TestSuite  # type: ignore[attr-defined]
from semgrep.external.junit_xml import to_xml_report_string  # type: ignore[attr-defined]
from semgrep.formatter.base import BaseFormatter
from semgrep.rule_match import RuleMatch


class JunitXmlFormatter(BaseFormatter):
    @staticmethod
    def _rule_match_to_test_case(rule_match: RuleMatch) -> TestCase:  # type: ignore
        test_case = TestCase(
            rule_match.id,
            file=str(rule_match.path),
            line=rule_match.start["line"],
            classname=str(rule_match.path),
        )
        test_case.add_failure_info(
            message=rule_match.message,
            output=rule_match.lines,
            failure_type=rule_match.severity,
        )
        return test_case

    def output(self) -> str:
        test_cases = [
            self._rule_match_to_test_case(rule_match)
            for rule_match in self.rule_matches
        ]
        ts = TestSuite("semgrep results", test_cases)
        return cast(str, to_xml_report_string([ts]))
