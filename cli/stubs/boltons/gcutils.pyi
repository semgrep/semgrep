from __future__ import annotations

from typing import Any
from typing import List
from typing import Type

def get_all(type_obj: type[bool], include_subtypes: bool = ...) -> list[Any]: ...

class GCToggler:
    def __enter__(self) -> None: ...
    def __exit__(self, exc_type: None, exc_val: None, exc_tb: None) -> None: ...
