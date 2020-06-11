from typing import Any
from typing import Dict
from typing import List

from semgrep.semgrep_types import BooleanRuleExpression


class Pattern:
    """
        Subunit of Rule that semgrep-core runs on
    """

    def __init__(
        self,
        rule_index: int,
        expression: BooleanRuleExpression,
        severity: str,
        languages: List[str],
    ) -> None:
        self._id = f"{rule_index}.{expression.pattern_id}"
        # if we don't copy an array (like `languages`), the yaml file will refer to it by reference (with an anchor)
        # which is nice and all but the semgrep YAML parser doesn't support that
        self._languages = languages.copy()
        self._severity = severity
        self._expression = expression
        self._pattern = expression.operand
        self._message = "<internalonly>"

    @property
    def languages(self) -> List[str]:
        return self._languages

    @property
    def expression(self) -> BooleanRuleExpression:
        return self._expression

    def to_json(self) -> Dict[str, Any]:
        return {
            "id": self._id,
            "pattern": self._pattern,
            "severity": self._severity,
            "languages": self._languages,
            "message": self._message,
        }

    def __repr__(self) -> str:
        return f"<{self.__class__.__name__} id={self._id}>"
