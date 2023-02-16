from __future__ import annotations

from typing import Any
from typing import Dict
from typing import Tuple
from typing import Union

def trace_print_hook(
    event: str,
    label: str,
    obj: dict[str, str] | bytes,
    attr_name: str,
    args: tuple = (),
    kwargs: dict[Any, Any] = {},
    result: Any = ...,
) -> None: ...
