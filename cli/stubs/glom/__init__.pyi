from __future__ import annotations

from types import DynamicClassAttribute
from typing import Any
from typing import Dict
from typing import Optional
from typing import Union

from .core import TType

T: TType

def glom(
    target: dict[str, Any], spec: TType | str | dict[str, Any], default: Any = None
) -> Any: ...
