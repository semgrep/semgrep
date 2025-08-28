#
# Copyright (c) 2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
"""Type-safe set operations

because mypy fails to report errors when using '.intersection()'
or '.union()' on two sets containing elements of different types.

The '-' operator for set difference works fine with mypy.
"""
from typing import Collection
from typing import FrozenSet
from typing import TypeVar

T = TypeVar("T")


def intersection(a: FrozenSet[T], b: Collection[T]) -> FrozenSet[T]:
    """Intersect two sets containing elements of the same type.

    If the elements in the two sets are not of the same type,
    Mypy will report an error, unlike a direct invocation
    of '.intersection()'.
    """
    return a.intersection(b)


def union(a: FrozenSet[T], b: Collection[T]) -> FrozenSet[T]:
    """Take the union of two sets containing elements of the same type.

    If the elements in the two sets are not of the same type,
    Mypy will report an error, unlike a direct invocation
    of '.union()'.
    """
    return a.union(b)
