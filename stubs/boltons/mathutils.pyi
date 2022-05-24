from typing import List, Optional, Union

IntOrFloat = Union[int, float]

def ceil(x: IntOrFloat, options: Optional[List[IntOrFloat]] = ...) -> int: ...
def clamp(
    x: IntOrFloat, lower: IntOrFloat = ..., upper: IntOrFloat = ...
) -> IntOrFloat: ...
def floor(x: IntOrFloat, options: Optional[List[IntOrFloat]] = ...) -> int: ...
