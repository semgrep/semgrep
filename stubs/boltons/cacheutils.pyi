from typing import Callable, Hashable, TypeVar, Union

ReturnType = TypeVar("ReturnType")

class cachedproperty(property): ...
class LRI: ...
class LRU: ...

def cachedmethod(
    cache: Union[LRI, LRU, str],
    scoped: bool = ...,
    typed: bool = ...,
    key: Callable[..., Hashable] = ...,
) -> Callable[[Callable[..., ReturnType]], Callable[..., ReturnType]]: ...
