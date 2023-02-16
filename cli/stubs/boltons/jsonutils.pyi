from __future__ import annotations

from io import TextIOWrapper
from typing import Iterator

def reverse_iter_lines(
    file_obj: TextIOWrapper, blocksize: int = ..., preseek: bool = ...
) -> Iterator[str]: ...

class JSONLIterator:
    def __init__(
        self,
        file_obj: TextIOWrapper,
        ignore_errors: bool = ...,
        reverse: bool = ...,
        rel_seek: float = ...,
    ) -> None: ...
    def __iter__(self) -> JSONLIterator: ...
    def _init_rel_seek(self) -> None: ...
    def next(self) -> dict[str, int]: ...
