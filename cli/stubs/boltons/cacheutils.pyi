from __future__ import annotations

from typing import Callable
from typing import Hashable
from typing import TypeVar
from typing import Union

ReturnType = TypeVar("ReturnType")

class cachedproperty(property): ...
class LRI: ...
class LRU: ...

def cachedmethod(
    cache: LRI | LRU | str,
    scoped: bool = ...,
    typed: bool = ...,
    key: Callable[..., Hashable] = ...,
) -> Callable[[Callable[..., ReturnType]], Callable[..., ReturnType]]: ...
