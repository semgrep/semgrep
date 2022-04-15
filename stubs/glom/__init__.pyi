from typing import Any, Dict, Optional, Union
from types import DynamicClassAttribute
from .core import TType

T: TType

def glom(
    target: Dict[str, Any], spec: Union[TType, str, Dict[str, Any]], default: Any = None
) -> Any: ...
