from __future__ import annotations

from typing import Any
from typing import Callable
from typing import Dict
from typing import Hashable
from typing import Iterable
from typing import Iterator
from typing import List
from typing import Optional
from typing import Tuple
from typing import TypeVar
from typing import Union

from boltons.dictutils import OrderedMultiDict

def backoff(
    start: int,
    stop: int | float,
    count: int | None = ...,
    factor: int | float = ...,
    jitter: int | float = ...,
) -> list[float | int] | list[float]: ...
def backoff_iter(
    start: int,
    stop: int | float,
    count: int | None = ...,
    factor: int | float = ...,
    jitter: int | float = ...,
) -> Iterator[float | int]: ...
def default_enter(path: tuple, key: str | int | None, value: Any) -> Any: ...
def default_exit(
    path: tuple,
    key: int | str | None,
    old_parent: Any,
    new_parent: frozenset | OrderedMultiDict | dict[str, None | int],
    new_items: Any,
) -> Any: ...
def first(
    iterable: list[int] | list[tuple | int],
    default: int | float | None = ...,
    key: Callable | None = ...,
) -> int | float | None: ...
def get_path(
    root: dict[str, list[str]] | list[str] | dict[str, str],
    path: tuple[str, int] | str | tuple[str] | tuple[int],
    default: Any = ...,
) -> str: ...
def remap(
    root: Any,
    visit: str | Callable = ...,
    enter: str | Callable = ...,
    exit: str | Callable = ...,
    **kwargs: Any,
) -> Any: ...
def research(
    root: dict[str, str], query: Callable | None = ..., reraise: bool = ...
) -> list[tuple[tuple[str], str]]: ...

class GUIDerator:
    def __init__(self, size: int = ...) -> None: ...
    def __next__(self) -> str: ...
    def reseed(self) -> None: ...

class SequentialGUIDerator:
    def __next__(self) -> str: ...
    def reseed(self) -> None: ...

Item = TypeVar("Item")
Items = Iterable[Item]

def chunked_iter(src: Items, size: int, **kw: Any) -> Iterable[Items]: ...

BucketKey_co = TypeVar("BucketKey_co", bound=Hashable, covariant=True)
BucketItem_co = TypeVar("BucketItem_co", covariant=True)
SourceItem_co = TypeVar("SourceItem_co", covariant=True)

def bucketize(
    src: Iterable[SourceItem_co],
    key: Callable[[SourceItem_co], BucketKey_co] = ...,
    value_transform: Callable[[SourceItem_co], BucketItem_co] | None = None,
    key_filter: Callable[[BucketKey_co], bool] | None = None,
) -> dict[BucketKey_co, list[BucketItem_co]]: ...
def partition(
    src: Iterable[SourceItem_co], key: Callable[[SourceItem_co], bool] = ...
) -> tuple[list[SourceItem_co], list[SourceItem_co]]: ...
