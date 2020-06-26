from typing import Any
from typing import Dict
from typing import List

from semgrep.semgrep_types import Language


class Equivalence:
    """
    Equivalence subunit.
    YAML spec. See https://github.com/returntocorp/semgrep/pull/362#issuecomment-607249377
    '''
    equivalences:
      - id: plus-commutative
        pattern: $X + $Y <==> $Y + $X
        languages: [python, javascript, c, go, java]
      - id: eq-to-no-eq
        pattern: $X == $X ==> $X != $X
        languages: [python, javascript, c, go, java]
    '''
    """

    def __init__(self, equiv_id: str, pattern: str, languages: List[Language]):
        self._id = equiv_id
        self._pattern = pattern
        self._languages = languages

    @property
    def pattern(self) -> str:
        return self._pattern

    def to_json(self) -> Dict[str, Any]:
        return {"id": self._id, "pattern": self._pattern, "languages": self._languages}
