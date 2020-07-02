from typing import Any
from typing import Dict
from typing import Optional

from semgrep.rule_lang import Span
from semgrep.semgrep_types import BooleanRuleExpression
from semgrep.semgrep_types import Language


class Pattern:
    """
        Subunit of Rule that semgrep-core runs on
    """

    def __init__(
        self,
        rule_index: int,
        expression: BooleanRuleExpression,
        severity: str,
        language: Language,
        span: Optional[Span],
    ) -> None:
        self._id = f"{rule_index}.{expression.pattern_id}"
        self._language = language
        self._severity = severity
        self._expression = expression
        self._pattern = expression.operand
        self._span = span

    @property
    def span(self) -> Optional[Span]:
        return self._span

    @property
    def language(self) -> Language:
        return self._language

    @property
    def expression(self) -> BooleanRuleExpression:
        return self._expression

    def to_json(self) -> Dict[str, Any]:
        # Note languages is a list as this is what semgrep-core expects
        return {
            "id": self._id,
            "pattern": self._pattern,
            "severity": self._severity,
            "languages": [self._language],
            "message": "<internalonly>",
        }

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} id={self._id}>"
