from itertools import chain, islice
from typing import Any, Generic, Iterator, List, Optional, TypeVar, Union

T = TypeVar("T")

class IndexedSet(Generic[T]):
    def __contains__(self, item: T) -> bool: ...
    def __eq__(self, other: IndexedSet) -> bool: ...  # type: ignore
    def __getitem__(self, index: Union[int, slice]) -> Union[IndexedSet, int]: ...
    def __iand__(self, *others: Any) -> IndexedSet: ...
    def __init__(
        self,
        other: Optional[Union[range, chain, IndexedSet, islice, List[T]]] = ...,
    ) -> None: ...
    def __ior__(self, *others: Any) -> IndexedSet: ...
    def __iter__(self) -> Iterator[Any]: ...
    def __len__(self) -> int: ...
    def __reversed__(self) -> Iterator[Any]: ...
    def _add_dead(self, start: int, stop: None = ...) -> None: ...
    def _compact(self) -> None: ...
    def _cull(self) -> None: ...
    @property
    def _dead_index_count(self) -> int: ...
    def _get_real_index(self, index: int) -> int: ...
    def add(self, item: T) -> None: ...
    def difference(self, *others: Any) -> IndexedSet: ...
    def discard(self, item: T) -> None: ...
    @classmethod
    def from_iterable(cls, it: Union[chain, islice]) -> IndexedSet: ...
    def intersection(self, *others: Any) -> IndexedSet: ...
    def intersection_update(self, *others: Any) -> None: ...
    def iter_slice(
        self, start: Optional[int], stop: int, step: Optional[int] = ...
    ) -> islice: ...
    def pop(self, index: Optional[int] = ...) -> T: ...
    def remove(self, item: T) -> None: ...
    def symmetric_difference(self, *others: Any) -> IndexedSet: ...
    def union(self, *others: Any) -> IndexedSet: ...
    def update(self, *others: Any) -> None: ...
