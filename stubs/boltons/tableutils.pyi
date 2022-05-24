from itertools import islice
from typing import Any, Dict, List, Optional, Union

def escape_html(obj: Optional[Union[str, int]], maxlen: None = ...) -> str: ...
def to_text(obj: Optional[Union[str, int]], maxlen: None = ...) -> str: ...

class DictInputType:
    def get_entry(
        self, obj: Dict[str, Union[int, str]], headers: List[str]
    ) -> List[Union[int, str]]: ...
    def get_entry_seq(
        self, obj: List[Dict[str, Union[int, str]]], headers: List[str]
    ) -> List[List[Union[int, str]]]: ...
    def guess_headers(self, obj: Dict[str, Union[int, str]]) -> List[str]: ...

class InputType:
    def __init__(self, *a: Any, **kw: Any) -> None: ...

class Table:
    def __init__(
        self,
        data: Optional[
            Union[
                List[Union[List[str], List[Union[int, str]]]],
                List[List[Union[int, str]]],
                List[List[Union[Dict[str, str], None, str]]],
            ]
        ] = ...,
        headers: Any = ...,
        metadata: None = ...,
    ) -> None: ...
    def __len__(self) -> int: ...
    def __repr__(self) -> str: ...
    def _add_horizontal_html_lines(
        self, lines: List[str], headers: List[str], max_depth: int
    ) -> None: ...
    def _add_vertical_html_lines(
        self, lines: List[str], headers: List[str], max_depth: int
    ) -> None: ...
    def _fill(self) -> None: ...
    def _set_width(self, reset: bool = ...) -> None: ...
    def extend(
        self,
        data: Union[
            List[List[Union[Dict[str, str], None, str]]],
            List[Union[List[Union[int, str]], List[int]]],
            List[List[Union[int, str]]],
            islice,
        ],
    ) -> None: ...
    @classmethod
    def from_data(
        cls,
        data: Union[Dict[str, Union[int, str]], List[Dict[str, Union[int, str]]]],
        headers: Any = ...,
        max_depth: int = ...,
        **kwargs: Any,
    ) -> Table: ...
    @classmethod
    def from_dict(
        cls,
        data: Union[Dict[str, Union[int, str]], List[Dict[str, Union[int, str]]]],
        headers: Any = ...,
        max_depth: int = ...,
        metadata: None = ...,
    ) -> Table: ...
    def get_cell_html(self, value: Optional[Union[int, str]]) -> str: ...
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
