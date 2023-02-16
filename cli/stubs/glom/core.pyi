from __future__ import annotations

from typing import Any

class TType:
    def __getattr__(self, attr: str) -> Any: ...
    def __getitem__(self, item: Any) -> Any: ...
