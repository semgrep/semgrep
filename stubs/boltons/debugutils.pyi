from typing import (
    Any,
    Dict,
    Tuple,
    Union,
)

def trace_print_hook(
    event: str,
    label: str,
    obj: Union[Dict[str, str], bytes],
    attr_name: str,
    args: Tuple = (),
    kwargs: Dict[Any, Any] = {},
    result: Any = ...,
) -> None: ...
