import json
from typing import Any
from typing import Dict
from typing import List


class Rule:
    def __init__(self, raw: Dict[str, Any]):
        self._raw = raw

    @property
    def id(self) -> str:
        return str(self._raw["id"])

    @property
    def message(self) -> str:
        return str(self._raw["message"])

    @property
    def severity(self) -> str:
        return str(self._raw["severity"])

    @property
    def languages(self) -> List[str]:
        languages: List[str] = self._raw["languages"]
        return languages

    @property
    def raw(self) -> Dict[str, Any]:  # type: ignore
        return self._raw

    @classmethod
    def from_json(cls, rule_json: Dict[str, Any]) -> "Rule":  # type: ignore
        return cls(rule_json)

    def to_json(self) -> Dict[str, Any]:
        return self._raw

    def __repr__(self) -> str:
        return json.dumps(self.to_json())
