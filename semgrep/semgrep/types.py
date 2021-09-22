from typing import Any
from typing import Mapping
from typing import NewType

JsonObject = Mapping[str, Any]
RuleId = NewType("RuleId", str)
