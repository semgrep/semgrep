from __future__ import annotations

from typing import Any
from typing import Callable
from typing import TypeVar

from click_option_group import OptionGroup

_F = TypeVar("_F")

def group(
    name: str | None = None,
    *,
    help: str | None = None,
    cls: type[OptionGroup] | None = None,
    **kwargs: Any
) -> Callable[[_F], _F]: ...
def option(*args: Any, **kwargs: Any) -> Callable[[_F], _F]: ...
