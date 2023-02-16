from __future__ import annotations

from collections import OrderedDict
from operator import itemgetter
from typing import Dict
from typing import Type
from typing import Union

def exec_(
    code: str,
    global_env: dict[
        str,
        type[itemgetter] | str | type[OrderedDict] | type[property] | type[tuple],
    ],
) -> None: ...
