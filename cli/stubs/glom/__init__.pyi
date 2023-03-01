from typing import Any, Dict, Optional, Tuple, Union
from types import DynamicClassAttribute
from .core import TType

T: TType

def glom(
    target: object,
    spec: Union[TType, str, Dict[str, Any]],
    default: Any = None,
) -> Any: ...
