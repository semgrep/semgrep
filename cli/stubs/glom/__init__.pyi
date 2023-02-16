from __future__ import annotations

from typing import Any

from .core import TType

T: TType

def glom(
    target: dict[str, Any], spec: TType | str | dict[str, Any], default: Any = None
) -> Any: ...
