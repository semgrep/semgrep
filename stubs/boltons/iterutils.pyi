from typing import (
    Any,
    Callable,
    Dict,
    Hashable,
    Iterable,
    List,
    Optional,
    TypeVar,
    Sequence,
    Union,
)

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
def get_path(
    root: Iterable[Item], path: Sequence[Union[str, int]], default: Item
) -> Item: ...
