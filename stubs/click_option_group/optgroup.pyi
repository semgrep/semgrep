from typing import TypeVar, Callable, Any, Optional, Type
from click_option_group import OptionGroup

_F = TypeVar("_F")

def group(
    name: Optional[str] = None,
    *,
    help: Optional[str] = None,
    cls: Optional[Type["OptionGroup"]] = None,
    **kwargs: Any
) -> Callable[[_F], _F]: ...
def option(*args: Any, **kwargs: Any) -> Callable[[_F], _F]: ...
