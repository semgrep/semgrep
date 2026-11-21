# Python 3.15: PEP 810 explicit lazy imports.
lazy import json
lazy import os.path as path, collections as collections_alias
lazy from pathlib import Path
lazy from . import sibling
lazy from ..package import (
    first as renamed,
    second,
)
lazy from lazy import lazy as lazy_alias
lazy import \
    decimal
if enabled:
    lazy import optional_dependency
with context():
    lazy from tools import helper

# The soft keyword remains an identifier outside an import prefix.
lazy = 1
lazy(lazy=lazy)
obj.lazy
import lazy as lazy
from lazy import lazy

def lazy(lazy: int) -> int:
    return lazy
