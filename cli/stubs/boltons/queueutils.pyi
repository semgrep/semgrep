from typing import Any, Callable, List, Union

class BasePriorityQueue:
    def __init__(self, **kw: Any) -> None: ...
    def __len__(self) -> int: ...
    def _cull(self, raise_exc: bool = ...) -> None: ...
    def add(self, task: Callable, priority: None = ...) -> None: ...
    def pop(self, default: Any = ...) -> Callable: ...
    def remove(self, task: Callable) -> None: ...

class HeapPriorityQueue:
    @staticmethod
    def _pop_entry(
        backend: Union[
            List[Union[List[Any], List[Union[int, Callable]]]],
            List[List[Union[int, Callable]]],
        ]
    ) -> Union[List[Union[int, Callable]], List[Any]]: ...
    @staticmethod
    def _push_entry(
        backend: List[List[Any]], entry: List[Union[int, Callable]]
    ) -> None: ...

class SortedPriorityQueue:
    @staticmethod
    def _pop_entry(
        backend: Union[
            List[Union[List[Any], List[Union[int, Callable]]]],
            List[List[Union[int, Callable]]],
        ]
    ) -> Union[List[Union[int, Callable]], List[Any]]: ...
    @staticmethod
    def _push_entry(
        backend: List[List[Any]], entry: List[Union[int, Callable]]
    ) -> None: ...
