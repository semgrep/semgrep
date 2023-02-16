from __future__ import annotations

from typing import Any

def trace_print_hook(
    event: str,
    label: str,
    obj: dict[str, str] | bytes,
    attr_name: str,
    args: tuple = (),
    kwargs: dict[Any, Any] = {},  # noqa: B006
    result: Any = ...,
) -> None: ...
