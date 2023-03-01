from __future__ import absolute_import

from boltons.dictutils import OrderedMultiDict
from typing import (
    Any,
    Callable,
    Dict,
    Hashable,
    Iterable,
    Iterator,
    List,
    Optional,
    Tuple,
    TypeVar,
    Union,
)

def backoff(
    start: int,
    stop: Union[int, float],
    count: Optional[int] = ...,
    factor: Union[int, float] = ...,
    jitter: Union[int, float] = ...,
) -> Union[List[Union[float, int]], List[float]]: ...
def backoff_iter(
    start: int,
    stop: Union[int, float],
    count: Optional[int] = ...,
    factor: Union[int, float] = ...,
    jitter: Union[int, float] = ...,
) -> Iterator[Union[float, int]]: ...
def default_enter(path: Tuple, key: Optional[Union[str, int]], value: Any) -> Any: ...
def default_exit(
    path: Tuple,
    key: Optional[Union[int, str]],
    old_parent: Any,
    new_parent: Union[frozenset, OrderedMultiDict, Dict[str, Union[None, int]]],
    new_items: Any,
) -> Any: ...
def first(
    iterable: Union[List[int], List[Union[Tuple, int]]],
    default: Optional[Union[int, float]] = ...,
    key: Optional[Callable] = ...,
) -> Optional[Union[int, float]]: ...
def get_path(
    root: Union[Dict[str, List[str]], List[str], Dict[str, str]],
    path: Union[Tuple[Union[str, int], ...], str, Tuple[str, ...], Tuple[int, ...]],
    default: Any = ...,
) -> str: ...
def remap(
    root: Any,
    visit: Union[str, Callable] = ...,
    enter: Union[str, Callable] = ...,
    exit: Union[str, Callable] = ...,
    **kwargs: Any,
) -> Any: ...
def research(
    root: Dict[str, str], query: Optional[Callable] = ..., reraise: bool = ...
) -> List[Tuple[Tuple[str], str]]: ...

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
    value_transform: Optional[Callable[[SourceItem_co], BucketItem_co]] = None,
    key_filter: Optional[Callable[[BucketKey_co], bool]] = None,
) -> Dict[BucketKey_co, List[BucketItem_co]]: ...
def partition(
    src: Iterable[SourceItem_co], key: Callable[[SourceItem_co], bool] = ...
) -> Tuple[List[SourceItem_co], List[SourceItem_co]]: ...
