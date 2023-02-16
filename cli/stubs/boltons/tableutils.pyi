from __future__ import annotations

from itertools import islice
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Union

def escape_html(obj: str | int | None, maxlen: None = ...) -> str: ...
def to_text(obj: str | int | None, maxlen: None = ...) -> str: ...

class DictInputType:
    def get_entry(
        self, obj: dict[str, int | str], headers: list[str]
    ) -> list[int | str]: ...
    def get_entry_seq(
        self, obj: list[dict[str, int | str]], headers: list[str]
    ) -> list[list[int | str]]: ...
    def guess_headers(self, obj: dict[str, int | str]) -> list[str]: ...

class InputType:
    def __init__(self, *a: Any, **kw: Any) -> None: ...

class Table:
    def __init__(
        self,
        data: list[list[str] | list[int | str]] | list[list[int | str]] | list[list[dict[str, str] | None | str]] | None = ...,
        headers: Any = ...,
        metadata: None = ...,
    ) -> None: ...
    def __len__(self) -> int: ...
    def __repr__(self) -> str: ...
    def _add_horizontal_html_lines(
        self, lines: list[str], headers: list[str], max_depth: int
    ) -> None: ...
    def _add_vertical_html_lines(
        self, lines: list[str], headers: list[str], max_depth: int
    ) -> None: ...
    def _fill(self) -> None: ...
    def _set_width(self, reset: bool = ...) -> None: ...
    def extend(
        self,
        data: list[list[dict[str, str] | None | str]] | list[list[int | str] | list[int]] | list[list[int | str]] | islice,
    ) -> None: ...
    @classmethod
    def from_data(
        cls,
        data: dict[str, int | str] | list[dict[str, int | str]],
        headers: Any = ...,
        max_depth: int = ...,
        **kwargs: Any,
    ) -> Table: ...
    @classmethod
    def from_dict(
        cls,
        data: dict[str, int | str] | list[dict[str, int | str]],
        headers: Any = ...,
        max_depth: int = ...,
        metadata: None = ...,
    ) -> Table: ...
    def get_cell_html(self, value: int | str | None) -> str: ...
    def to_html(
        self,
        orientation: None = ...,
        wrapped: bool = ...,
        with_headers: bool = ...,
        with_newlines: bool = ...,
        with_metadata: bool = ...,
        max_depth: int = ...,
    ) -> str: ...
    def to_text(self, with_headers: bool = ..., maxlen: None = ...) -> str: ...
